
structure PSFSemantics = 
struct

open LangCommon
open LambdaPSF
structure S = SourceLang

datatype value	= V of (value,value) ValuesBase.valueF
structure Context = BasicContext (MainDict) (type t = value) 

structure Values = EmbedValues 
(struct
	type v = value
	type f = value
	type e = expr
	fun outof (V v) = v
	val into = V
end)

structure EvaluatorPSF = S.Evaluator (type t = value) (Values)
  
fun extendPattern g (P p) t = S.foldPattern (Context.extend, extendPattern, Values.untuple, Values.unroll, Stuck) g p t

fun evaluate env (E exp) = EvaluatorPSF.evalF env evaluate (extendPattern,Context.lookup) exp

end
