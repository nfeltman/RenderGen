
structure ErasureSemantics = 
struct

local
open LangCommon
open Lambda12
open SourceLang
structure V = ValuesBase
in

datatype value1	= V1 of (value1,cont,pattern12,expr1) V.valueF
				| V1next of value2
				| V1mono of valueM
				
and		value2	= V2 of (value2,cont,patternM,expr2) V.valueF
and		valueM	= VM of (valueM,cont,patternM,exprM) V.valueF
withtype   cont = (var, (value1,value2,valueM) Contexts.TripleContext.tripleEntry) Contexts.context

fun holdGeneral (V1 (V.VFprim i)) = V1next (V2 (V.VFprim i))
  | holdGeneral _ = raise Stuck

fun V1unwrap (V1 v) = v
  | V1unwrap _ = raise Stuck
fun unnext (V1next v) = v
  | unnext _ = raise Stuck
fun unmono (V1mono v) = v
  | unmono _ = raise Stuck
  
structure Values1 = EmbedValues (struct
	type v = value1
	type c = cont
	type r = pattern12
	type e = expr1
	val outof = V1unwrap
	val into = V1
end)
structure Values2 = EmbedValues (struct
	type v = value2
	type c = cont
	type r = patternM
	type e = expr2
	fun outof (V2 v) = v
	val into = V2
end)
structure ValuesM = EmbedValues (struct
	type v = valueM
	type c = cont
	type r = patternM
	type e = exprM
	fun outof (VM v) = v
	val into = VM
end)

structure Evaluator1 = Evaluator (Values1)
structure Evaluator2 = Evaluator (Values2)
structure EvaluatorM = Evaluator (ValuesM)

structure TC = Contexts.TripleContext

fun ext1 g (P p) t = foldPattern (TC.extendContext1, ext1, Values1.untuple, Stuck) g p t
  | ext1 g (Pmono p) t = ext3 g p (unmono t)
and ext2 g (PM p) t = foldPattern (TC.extendContext2, ext2, Values2.untuple, Stuck) g p t
and ext3 g (PM p) t = foldPattern (TC.extendContext3, ext3, ValuesM.untuple, Stuck) g p t

fun eval1 env (E1 exp) = Evaluator1.evalF env eval1 (ext1,TC.lookup1) exp
  | eval1 env (E1next e) = V1next (eval2 env e)
  | eval1 env (E1hold e) = holdGeneral (eval1 env e)
  | eval1 env (E1mono e) = V1mono (evalM env e)
  | eval1 env (E1pushPrim e) = Values1.makeprim (ValuesM.unprim (unmono (eval1 env e)))
  | eval1 env (E1pushProd e) = Values1.maketuple (map V1mono (ValuesM.untuple (unmono (eval1 env e))))
  | eval1 env (E1pushSum e) = 
		let 
			val (i,v) = ValuesM.uninj (unmono (eval1 env e))
		in
			Values1.makeinj (i, V1mono v)
		end
  | eval1 env (E1pushArr e) = 
		let 
			val (c,b) = ValuesM.unlam (unmono (eval1 env e))
			val y = Variable.newvar "y"
		in
			Values1.makelam (c, (Pmono (PM (Pvar y)), 
					E1mono ( EM (Flet (EM (Fvar y),b)))))
		end
		
and evalM env (EM exp) = EvaluatorM.evalF env evalM (ext3,TC.lookup3) exp
and eval2 env (E2 exp) = Evaluator2.evalF env eval2 (ext2,TC.lookup2) exp
  | eval2 env (E2prev e) = unnext (eval1 env e)

end
end
