
structure PSFSemantics = 
struct

open LangCommon
open Contexts
open LambdaPSF
structure S = SourceLang

datatype value	= V of (value,(var, value) context,pattern,expr) ValuesBase.valueF

structure Values = EmbedValues 
(struct
	type v = value
	type c = (var, value) context
	type r = pattern
	type e = expr
	fun outof (V v) = v
	val into = V
end)

structure EvaluatorPSF = S.Evaluator (Values)
  
fun extendPattern g (P p) t = S.foldPattern (extendContext, extendPattern, Values.untuple, Stuck) g p t

fun evaluate env (E exp) = EvaluatorPSF.evalF env evaluate (extendPattern,lookup) exp

end
