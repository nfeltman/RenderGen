
structure PSFSemantics = 
struct

open LangCommon
open LambdaPSF
structure S = SourceLang

datatype value	= V of (value,value MainDict.cont,pattern,expr) ValuesBase.valueF
structure Context = BasicContext (MainDict) (type t = value) 

structure Values = EmbedValues 
(struct
	type v = value
	type c = Context.cont
	type r = pattern
	type e = expr
	fun outof (V v) = v
	val into = V
end)

structure EvaluatorPSF = S.Evaluator (Values)
  
fun extendPattern g (P p) t = S.foldPattern (Context.extend, extendPattern, Values.untuple, Stuck) g p t

fun evaluate env (E exp) = EvaluatorPSF.evalF env evaluate (extendPattern,Context.lookup) exp

end
