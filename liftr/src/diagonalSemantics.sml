
structure DiagonalSemantics = 
struct

open LangCommon

datatype expr1	= E1var of var
				| E1unit
				| E1tuple of expr1 * expr1
				| E1pi of LR * expr1
				| E1if of expr1 * expr1 * expr1
				| E1let of expr1 * (var * expr1)
				| E1next of expr2
				| E1error
				| E1binop of Prims.binops * expr1 * expr1
				| E1splitBlock of value * var
				
and expr2		= E2var of var
				| E2unit
				| E2tuple of expr2 * expr2
				| E2pi of LR * expr2
				| E2if of expr2 * expr2 * expr2
				| E2let of expr2 * (var * expr2)
				| E2prev of expr1
				| E2error
				| E2binop of Prims.binops * expr2 * expr2
				
and value		= Vint of int
				| Vbool of bool
				| Vunit
				| Vtuple of value * value
				
datatype expr	= Evar of var
				| Eunit
				| Etuple of expr * expr
				| Epi of LR * expr
				| Eif of expr * expr * expr
				| Elet of expr * (var * expr)
				| Ebinop of Prims.binops * expr * expr
				| Eerror
				
exception Stuck

structure S = Lambda12
fun inj1 exp = 
	case exp of 
	  S.E1var v => E1var v
	| S.E1unit => E1unit
	| S.E1tuple (e1, e2) => E1tuple (inj1 e1, inj1 e2)
	| S.E1pi (side, e) => E1pi (side, inj1 e)
	| S.E1if (e1, e2, e3) => E1if (inj1 e1, inj1 e2, inj1 e3)
	| S.E1let (e1,(x,e2)) => E1let (inj1 e1,(x,inj1 e2))
	| S.E1binop (bo,e1,e2) => E1binop (bo,inj1 e1,inj1 e2)
	| S.E1error _ => E1error
	| S.E1next e => E1next (inj2 e)

and inj2 exp = 
	case exp of 
	  S.E2var v => E2var v
	| S.E2unit => E2unit
	| S.E2tuple (e1, e2) => E2tuple (inj2 e1, inj2 e2)
	| S.E2pi (side, e) => E2pi (side, inj2 e)
	| S.E2if (e1, e2, e3) => E2if (inj2 e1, inj2 e2, inj2 e3)
	| S.E2let (e1,(x,e2)) => E2let (inj2 e1,(x,inj2 e2))
	| S.E2binop (bo,e1,e2) => E2binop (bo,inj2 e1,inj2 e2)
	| S.E2error _ => E2error
	| S.E2prev e => E2prev (inj1 e)
	
fun eval1 exp = 
	case exp of 
	  E1var v => raise Stuck
	| E1unit => (Vunit, Eunit)
	| E1tuple (e1, e2) => (
		case (eval1 e1, eval1 e2) of
		((v1,r1),(v2,r2)) => (Vtuple (v1, v2), Etuple (r1,r2)))
	| E1pi (side, e) => (
		case (side,eval1 e) of
		  (Left, (Vtuple (v1,_), r)) => (v1, Epi (Left, r))
		| (Right, (Vtuple (_,v2), r)) => (v2, Epi (Right, r))
		| _ => raise Stuck)
	| E1if (e1, e2, e3) => 
		let
			val ((v,r2), r1) =	
				case eval1 e1 of
				  (Vbool true, r1) => (eval1 e2, r1)
				| (Vbool false, r1) => (eval1 e3, r1)
				| _ => raise Stuck
		in
			(v,Epi(Right, Etuple (r1,r2)))
		end
	| E1let (e1,(x,e2)) => raise Stuck
	| E1binop (bo,e1,e2) => raise Stuck
	| E1error => raise Stuck
	| E1next e => (Vunit, trace2 e)
	| E1splitBlock (v,var) => (v, Evar var)
	
and trace2 exp = 
	case exp of 
	  E2var v => Evar v
	| E2unit => Eunit
	| E2tuple (e1, e2) => Etuple (trace2 e1, trace2 e2)
	| E2pi (side, e) => Epi (side, trace2 e)
	| E2if (e1, e2, e3) => Eif (trace2 e1, trace2 e2, trace2 e3)
	| E2let (e1,(x,e2)) => Elet (trace2 e1, (x, trace2 e2))
	| E2binop (bo,e1,e2) => Ebinop (bo, trace2 e1, trace2 e2)
	| E2error => raise Stuck
	| E2prev e => (
		case eval1 e of
		  (Vunit, r) => r
		| _ => raise Stuck)

fun eval2 exp = 
	case exp of 
	  Evar v => raise Stuck
	| Eunit => Vunit
	| Etuple (e1, e2) => Vtuple (eval2 e1, eval2 e2)
	| Epi (side, e) => (
		case (side, eval2 e) of
		  (Left, Vtuple (v1,_)) => v1
		| (Right, Vtuple (_,v2)) => v2
		| _ => raise Stuck)
	| Eif (e1, e2, e3) => (
		case eval2 e1 of
		  Vbool true => eval2 e2
		| Vbool false => eval2 e3
		| _ => raise Stuck)
	| Elet (e,b) => evalBranch (eval2 e) b
	| Ebinop (bo,e1,e2) => raise Stuck
	| Eerror => raise Stuck

and evalBranch v (x,e) = raise Stuck

end
