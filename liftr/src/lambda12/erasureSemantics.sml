
structure ErasureSemantics = 
struct

local
open LangCommon
open Lambda12
open SourceLang
structure V = ValuesBase
in

datatype value1	= V1 of (value1,cont,var pattern,expr1) V.valueF
				| V1next of value2
				
and		value2	= V2 of (value2,cont,var pattern,expr2) V.valueF
withtype   cont = (var, (value1,value2) Contexts.DoubleContext.doubleEntry) Contexts.context


fun holdGeneral (V1 (V.VFprim i)) = V1next (V2 (V.VFprim i))
  | holdGeneral _ = raise Stuck

fun V1unwrap (V1 v) = v
  | V1unwrap _ = raise Stuck
fun unnext (V1next v) = v
  | unnext _ = raise Stuck
  
fun eval1 env (E1 exp) = evalF env eval1 Contexts.DoubleContext.extendLookup1 V1 V1unwrap exp
  | eval1 env (E1next e) = V1next (eval2 env e)
  | eval1 env (E1hold e) = holdGeneral (eval1 env e)
	
and eval2 env (E2 exp) = evalF env eval2 Contexts.DoubleContext.extendLookup2 V2 (fn (V2 v) => v) exp
  | eval2 env (E2prev e) = unnext (eval1 env e)

end
end
