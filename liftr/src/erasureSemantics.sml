
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

datatype envEntry = Val1 of value1 | Val2 of value2
				
exception Stuck

fun unval1 (Val1 v) = v
  | unval1 _ = raise Stuck
fun unval2 (Val2 v) = v
  | unval2 _ = raise Stuck
fun untuple1 (V1tuple v) = v
  | untuple1 _ = raise Stuck
fun unbool1 (V1bool b) = b
  | unbool1 _ = raise Stuck
fun untuple2 (V2tuple v) = v
  | untuple2 _ = raise Stuck
fun unbool2 (V2bool b) = b
  | unbool2 _ = raise Stuck

fun eval1 env exp = 
	let
		val eval = eval1 env
		fun evalBranch value (var,e) = eval1 (extendContext env var (Val1 value)) e
	in
		case exp of 
		  E1var v => unval1 (lookup env v)
		| E1unit => V1unit
		| E1tuple (e1, e2) => V1tuple (eval e1, eval e2)
		| E1pi (side, e) => (case side of Left => #1 | Right => #2) (untuple1 (eval e))
		| E1if (e1, e2, e3) => eval (if unbool1 (eval e1) then e2 else e3)
		| E1let (e, b) => evalBranch (eval e) b
		| E1binop (bo,e1,e2) => raise Stuck
		| E1error t => raise Stuck
		| E1next e => V1next (eval2 env e)
	end
	
and eval2 env exp = 
	let
		val eval = eval2 env
		fun evalBranch value (var,e) = eval2 (extendContext env var (Val2 value)) e
	in	
		case exp of 
		  E2var v => unval2 (lookup env v)
		| E2unit => V2unit
		| E2tuple (e1, e2) => V2tuple (eval e1, eval e2)
		| E2pi (side, e) => (case side of Left => #1 | Right => #2) (untuple2 (eval e))
		| E2if (e1, e2, e3) => eval (if unbool2 (eval e1) then e2 else e3)
		| E2let (e, b) => evalBranch (eval e) b
		| E2binop (bo,e1,e2) => raise Stuck
		| E2error t => raise Stuck
		| E2prev e => (
			case eval1 env e of
			  V1next v => v
			| _ => raise Stuck)
	end

end
