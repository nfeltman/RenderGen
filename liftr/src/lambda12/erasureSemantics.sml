
structure ErasureSemantics = 
struct

open LangCommon
open Lambda12

datatype value1	= V1 of (value1,cont,var pattern,expr1) valueF
				| V1next of value2
				
and		value2	= V2 of (value2,cont,var pattern,expr2) valueF
withtype   cont = (var, (value1,value2) doubleEntry) context

fun V1unwrap (V1 v) = v
  | V1unwrap _ = raise Stuck
fun unnext (V1next v) = v
  | unnext _ = raise Stuck
  
fun eval1 env (E1 exp) = evalF env eval1 extendLookup1 V1 V1unwrap exp
  | eval1 env (E1next e) = V1next (eval2 env e)
  | eval1 env (E1hold e) = (V1next o V2 o VFint o unint o V1unwrap o (eval1 env)) e
	
and eval2 env (E2 exp) = evalF env eval2 extendLookup2 V2 (fn (V2 v) => v) exp
  | eval2 env (E2prev e) = unnext (eval1 env e)

end
