
structure DiagonalSemantics = 
struct

open LangCommon
open Lambda12
structure P = Prims.PrimEval
				
datatype value	= V of value valueF
datatype expr	= E of (expr, unit) exprF

fun unV (V v) = v
	
fun chain r1 r2 = Fpi(Right, E (Ftuple (r1,r2)))
fun eval1 env (E1 exp) = 
	let
		fun map1 f (a,b) = (f a, b)
		fun map2 f (a,b) = (a, f b)
		val eval = eval1 env
		fun evalBranch value (var,e) = eval1 (extendContext env var value) e
		val (v,r) = 
			case exp of 
			  Fvar v => lookup env v
			| Funit => (V VFunit, Funit)
			| Fint i => (V (VFint i), Funit)
			| Fbool b => (V (VFbool b), Funit)
			| Ftuple (e1, e2) => bimap (V o VFtuple) Ftuple (trn (eval e1, eval e2))
			| Fpi (side, e) => (
				case (side, map1 (untuple o unV) (eval e)) of
				  (Left, ((v1,_), r)) => (v1, Fpi (Left, r))
				| (Right, ((_,v2), r)) => (v2, Fpi (Right, r)))
			| Fif (e1, e2, e3) => 
				let
					val (v1,r1) = map1 (unbool o unV) (eval e1)
				in
					map2 (chain r1) (eval (if v1 then e2 else e3))
				end
			| Flet (e1,(x,e2)) => 
				let 
					val (v1,r1) = eval e1
				in
					map2 (fn r2 => Flet (r1, (x,r2))) (evalBranch (v1, Fvar x) (x,e2))
				end
			| Fbinop (bo,e1,e2) => 
				let
					val (v1,r1) = eval e1
					val (v2,r2) = eval e2
					val convertP = convertPrim o unV
				in
					(V (unconvertPrim (P.evalPrim (bo, convertP v1, convertP v2))), chain r1 r2)
				end
			| Ferror _ => raise Stuck
		in
			(v, E r)
		end
  | eval1 env (E1next e) = (V VFunit, trace2 env e)
  | eval1 env (E1hold e) = (case eval1 env e of (V v,r) => (V VFunit, E (chain r ((E o Fint o unint) v))))
	
and trace2 env (E2 exp) = E (mapExpr (trace2 env) (fn _ => ()) exp)
  | trace2 env (E2prev e) = (case eval1 env e of (V VFunit, r) => r | _ => raise Stuck)

fun eval2 env (E exp) = evalF env eval2 (extendContext,lookup) V unV exp

end
