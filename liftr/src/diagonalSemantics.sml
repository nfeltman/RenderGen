
structure DiagonalSemantics = 
struct

open LangCommon
open Lambda12
structure P = Prims.PrimEval
				
datatype value	= Vint of int
				| Vbool of bool
				| Vunit
				| Vtuple of value * value
				
datatype expr	= Evar of var
				| Eunit
				| Eint of int
				| Ebool of bool
				| Etuple of expr * expr
				| Epi of LR * expr
				| Eif of expr * expr * expr
				| Elet of expr * (var * expr)
				| Ebinop of Prims.binops * expr * expr
				| Eerror

exception Stuck

fun map1 f (a,b) = (f a, b)

fun untuple (Vtuple v) = v
  | untuple _ = raise Stuck
fun unbool (Vbool b) = b
  | unbool _ = raise Stuck
fun unint (Vint i) = i
  | unint _ = raise Stuck
  
fun convertPrim (Vint i) = P.Vint i
  | convertPrim (Vbool b) = P.Vbool b
  | convertPrim _ = raise Stuck
fun unconvertPrim (P.Vint i) = Vint i
  | unconvertPrim (P.Vbool b) = Vbool b

	
fun eval1 env (E1 exp) = 
	let
		val eval = eval1 env
		fun evalBranch value (var,e) = eval1 (extendContext env var value) e
	in
		case exp of 
		  Fvar v => lookup env v
		| Funit => (Vunit, Eunit)
		| Fint i => (Vint i, Eunit)
		| Fbool b => (Vbool b, Eunit)
		| Ftuple (e1, e2) => (
			case (eval e1, eval e2) of
			((v1,r1),(v2,r2)) => (Vtuple (v1, v2), Etuple (r1,r2)))
		| Fpi (side, e) => (
			case (side, map1 untuple (eval e)) of
			  (Left, ((v1,_), r)) => (v1, Epi (Left, r))
			| (Right, ((_,v2), r)) => (v2, Epi (Right, r)))
		| Fif (e1, e2, e3) => 
			let
				val (v1,r1) = map1 unbool (eval e1)
				val (v, r2) = eval (if v1 then e2 else e3)	
			in
				(v, Epi(Right, Etuple (r1,r2)))
			end
		| Flet (e1,(x,e2)) => 
			let 
				val (v1,r1) = eval e1
				val (v2,r2) = evalBranch (v1,Evar x) (x,e2)
			in
				(v2, Elet (r1, (x,r2)))
			end
		| Fbinop (bo,e1,e2) => 
			let
				val (v1,r1) = eval e1
				val (v2,r2) = eval e2
			in
				(unconvertPrim (P.evalPrim (bo, convertPrim v1, convertPrim v2)), Epi(Right, Etuple (r1,r2)))
			end
		| Ferror _ => raise Stuck
	end
  | eval1 env (E1next e) = (Vunit, trace2 env e)
  | eval1 env (E1hold e) = (case eval1 env e of (v,r) => (Vunit, Epi(Right, Etuple (r, Eint (unint v)))))
	
and trace2 env (E2 exp) = 
	let
		val trace = trace2 env
	in
		case exp of 
		  Fvar v => Evar v
		| Funit => Eunit
		| Fint i => Eint i
		| Fbool b => Ebool b
		| Ftuple (e1, e2) => Etuple (trace e1, trace e2)
		| Fpi (side, e) => Epi (side, trace e)
		| Fif (e1, e2, e3) => Eif (trace e1, trace e2, trace e3)
		| Flet (e1,(x,e2)) => Elet (trace e1, (x, trace e2))
		| Fbinop (bo,e1,e2) => Ebinop (bo, trace e1, trace e2)
		| Ferror _ => Eerror
	end
  | trace2 env (E2prev e) = (case eval1 env e of (Vunit, r) => r | _ => raise Stuck)

fun eval2 env exp = 
	let
		val eval = eval2 env
		fun evalBranch value (var,e) = eval2 (extendContext env var value) e
	in
		case exp of 
		  Evar v => lookup env v
		| Eunit => Vunit
		| Eint i => Vint i
		| Ebool b => Vbool b
		| Etuple (e1, e2) => Vtuple (eval e1, eval e2)
		| Epi (side, e) => (case side of Left => #1 | Right => #2) (untuple (eval e))
		| Eif (e1, e2, e3) => eval (if unbool (eval e1) then e2 else e3)
		| Elet (e,b) => evalBranch (eval e) b
		| Ebinop (bo,e1,e2) => unconvertPrim (P.evalPrim (bo, convertPrim (eval e1), convertPrim (eval e2)))
		| Eerror => raise Stuck
	end

and evalBranch v (x,e) = raise Stuck

end
