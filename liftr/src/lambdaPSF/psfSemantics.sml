
structure PSFSemantics = 
struct

open LangCommon
open Contexts
open LambdaPSF
structure S = SourceLang

datatype value	= V of (value,(var, value) context,var S.pattern,expr) ValuesBase.valueF
fun unV (V v) = v
fun evaluate env (E exp) = S.evalF env evaluate (extendContext,lookup) V unV exp

fun extendPattern g p v = S.forPattern (extendContext, ValuesBase.untuple o unV, Stuck) g p v

end
