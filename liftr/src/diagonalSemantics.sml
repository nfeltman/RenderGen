
structure DiagonalSemantics = 
struct

open LangCommon
open Lambda12
structure P = Prims.PrimEval

(* first stage values *)				
datatype value1	= V1 of (value1,var,expr1) valueF

(* second stage values/expressions *)
datatype expr	= E of (expr,var,unit) exprF
datatype value2	= V2 of (value2,var,expr) valueF

fun unV1 (V1 v) = v
fun unV2 (V2 v) = v
	
fun chain2 r1 r2 = E (Fpi(Right, E (Ftuple (r1,r2))))
fun chain3 r1 r2 r3 = chain2 r1 (chain2 r2 r3)
fun eval1 env (E1 exp) = 
	let
		fun map1 f (a,b) = (f a, b)
		fun map2 f (a,b) = (a, f b)
		val (eval,V,unV) = (eval1 env, V1, unV1)
		fun evalBranch (v,r) (x,e) = 
			let 
				val (u,q) = eval1 (extendContext env x v) e 
			in 
				(u,E (Flet (r,(x,q))))
			end
		in
			case exp of 
			  Fvar v => (lookup env v, E (Fvar v))
			| Funit => (V VFunit, E Funit)
			| Fint i => (V (VFint i), E Funit)
			| Fbool b => (V (VFbool b), E Funit)
			| Flam (_, b) => (V (VFlam b), E Funit)
			| Fapp (e1,e2) => 
				let 
					val (branch, r1) = map1 (unlam o unV) (eval e1)
				in
					map2 (chain2 r1) (evalBranch (eval e2) branch)
				end
			| Ftuple (e1, e2) => bimap (V o VFtuple) (E o Ftuple) (trn (eval e1, eval e2))
			| Fpi (side, e) => (
				case (side, map1 (untuple o unV) (eval e)) of
				  (Left, ((v1,_), r)) => (v1, E (Fpi (Left, r)))
				| (Right, ((_,v2), r)) => (v2, E (Fpi (Right, r))))
			| Finj (side, _, e) => map1 (fn x => V (VFinj (side,x))) (eval e)
			| Fcase (e, (x1,e1), (x2,e2)) => 
				let
					val ((side,v),r) = map1 (uninj o unV) (eval e)
				in
					case side of
					  Left  => evalBranch (v,r) (x1,e1)
					| Right => evalBranch (v,r) (x2,e2)
				end
			| Fif (e1, e2, e3) => 
				let
					val (v,r) = map1 (unbool o unV) (eval e1)
				in
					map2 (chain2 r) (eval (if v then e2 else e3))
				end
			| Flet (e1,(x,e2)) => evalBranch (eval e1) (x,e2)
			| Fbinop (bo,e1,e2) => 
				let
					val (v1,r1) = eval e1
					val (v2,r2) = eval e2
					val convertP = convertPrim o unV
				in
					(V (unconvertPrim (P.evalPrim (bo, convertP v1, convertP v2))), chain3 r1 r2 (E Funit))
				end
			| Ferror _ => raise Stuck
		end
  | eval1 env (E1next e) = (V1 VFunit, trace2 env e)
  | eval1 env (E1hold e) = (case eval1 env e of (V1 v,r) => (V1 VFunit, chain2 r ((E o Fint o unint) v)))
	
and trace2 env (E2 exp) = E (mapExpr (trace2 env) (fn _ => ()) exp)
  | trace2 env (E2prev e) = (case eval1 env e of (V1 VFunit, r) => r | _ => raise Stuck)

fun eval2 env (E exp) = evalF env eval2 (extendContext,lookup) V2 unV2 exp

end
