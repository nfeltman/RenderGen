
structure ErasureSemantics = 
struct

open LangCommon
open Lambda12

datatype value1	= V1int of int
				| V1bool of bool
				| V1unit
				| V1tuple of value1 * value1
				| V1next of value2
				
and value2		= V2int of int
				| V2bool of bool
				| V2unit
				| V2tuple of value2 * value2

exception Stuck
				
fun eval1 exp = 
	case exp of 
	  E1var v => raise Stuck
	| E1unit => V1unit
	| E1tuple (e1, e2) => V1tuple (eval1 e1, eval1 e2)
	| E1pi (side, e) => (
		case (side,eval1 e) of
		  (Left, V1tuple (v1,_)) => v1
		| (Right, V1tuple (_,v2)) => v2
		| _ => raise Stuck)
	| E1binop (bo,e1,e2) => raise Stuck
	| E1error t => raise Stuck
	| E1next e => V1next (eval2 e)
	
and eval2 exp = 
	case exp of 
	  E2var v => raise Stuck
	| E2unit => V2unit
	| E2tuple (e1, e2) => V2tuple (eval2 e1, eval2 e2)
	| E2pi (side, e) => (
		case (side, eval2 e) of
		  (Left, V2tuple (v1,_)) => v1
		| (Right, V2tuple (_,v2)) => v2
		| _ => raise Stuck)
	| E2binop (bo,e1,e2) => raise Stuck
	| E2error t => raise Stuck
	| E2prev e => (
		case eval1 e of
		  V1next v => v
		| _ => raise Stuck)
		
end
