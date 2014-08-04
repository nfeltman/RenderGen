
structure PSFSemantics = 
struct

open LangCommon
open Contexts
open LambdaPSF
structure P = Prims.PrimEval

datatype value	= V of (value,(var, value) context,var S.pattern,expr) S.valueF
fun unV (V v) = v
fun evaluate env (E exp) = S.evalF env evaluate (extendContext,lookup) V unV exp

fun extendPattern g p v = S.forPattern (extendContext, S.untuple o unV, Stuck) g p v

end
