
structure DiagonalSemantics = 
struct

open LangCommon
open Lambda12
open LambdaPSF

datatype value1	= Vint of int
				| Vbool of bool
				| Vunit
				| Vtuple of value1 * value1

exception Stuck
				
fun eval1 exp = 
	case exp of 
	  E1var v => raise Stuck
	| E1unit => (Vunit, Etuple[])
	| E1tuple (e1, e2) => (
		case (eval1 e1, eval1 e2) of
		((v1,r1),(v2,r2)) => (Vtuple (v1, v2), Etuple [r1,r2]))
	| E1pi (side, e) => (
		case (side,eval1 e) of
		  (Left, (Vtuple (v1,_), r)) => (v1, Epi (0, r))
		| (Right, (Vtuple (_,v2), r)) => (v2, Epi (1, r))
		| _ => raise Stuck)
	| E1binop (bo,e1,e2) => raise Stuck
	| E1error t => raise Stuck
	| E1next e => (Vunit, trace2 e)
	
and trace2 exp = 
	case exp of 
	  E2var v => Evar v
	| E2unit => Etuple[]
	| E2tuple (e1, e2) => Etuple [trace2 e1, trace2 e2]
	| E2pi (side, e) => Epi (case side of Left => 0 | Right => 1, trace2 e)
	| E2binop (bo,e1,e2) => Ebinop (bo, trace2 e1, trace2 e2)
	| E2error t => raise Stuck
	| E2prev e => (
		case eval1 e of
		  (Vunit, r) => r
		| _ => raise Stuck)

end
