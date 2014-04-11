
structure PSFSemantics = 
struct

open LangCommon
open Contexts
open LambdaPSF
structure P = Prims.PrimEval
				
datatype value	= Vint of int
				| Vbool of bool
				| Vtuple of value list
				| Vinj of LR * value
				| Vlam of (var, value) context * (var * unit expr)

exception Stuck

fun convertPrim (Vint i) = P.Vint i
  | convertPrim (Vbool b) = P.Vbool b
  | convertPrim _ = raise Stuck

fun unconvertPrim (P.Vint i) = Vint i
  | unconvertPrim (P.Vbool b) = Vbool b
		
fun evaluate env exp = 
	let
		val eval = evaluate env
		fun evalBranchE v (env,(x,e)) = evaluate (extendContext env x v) e
		fun evalBranch v b = evalBranchE v (env, b)
	in
		case exp of 
		  Evar v => lookup env v
		| Eint i => Vint i
		| Ebool b => Vbool b
		| Elam (_,b) => Vlam (env,b)
		| Eapp (e1,e2) => (case (eval e1, eval e2) of
			  (Vlam eb, v) => evalBranchE v eb
			| _ => raise Stuck)
		| Etuple es => Vtuple (map eval es)
		| Epi (index, e) => (
			case eval e of
			  Vtuple es => List.nth (es, index)
			| _ => raise Stuck)
		| Einj (side,_,e) => Vinj (side, eval e)
		| Ecase (e1, b1, b2) => (
			case eval e1 of
			  Vinj (Left, v) => evalBranch v b1
			| Vinj (Right, v) => evalBranch v b2
			| _ => raise Stuck)
		| Eif (e1, e2, e3) => (
			case eval e1 of
			  Vbool true => eval e2
			| Vbool false => eval e3
			| _ => raise Stuck)
		| Elet (e,b) => evalBranch (eval e) b
		| Ebinop (bo,e1,e2) => unconvertPrim (P.evalPrim (bo, convertPrim (eval e1), convertPrim (eval e2)))
		| Eroll _ => raise Stuck
		| Eunroll _ => raise Stuck
		| Eerror t => raise Stuck
	end
end
