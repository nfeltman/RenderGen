
structure PSFSemantics = 
struct

open LangCommon
open Contexts
open LambdaPSF
structure S = SourceLang

datatype value	= V of (value,(var, value) context,var S.pattern,expr) ValuesBase.valueF
				| Vdummy

fun unV (V v) = v
  | unV _ = raise Stuck

structure Values = EmbedValues (struct
	type v = value
	type c = (var, value) context
	type r = var S.pattern
	type e = expr
	val outof = unV
	val into = V
end)

structure EvaluatorPSF = S.Evaluator (Values)

fun evaluate env (E exp) = EvaluatorPSF.evalF env evaluate (extendContext,lookup) exp
  | evaluate env Edummy = Vdummy

fun extendPattern g p v = S.forPattern (extendContext, ValuesBase.untuple o unV, Stuck) g p v

end
