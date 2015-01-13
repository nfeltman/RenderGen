
structure ErasureSemantics = 
struct

local
open LangCommon
open Lambda12
open SourceLang
structure V = ValuesBase
in

structure Values = 
struct
	datatype value1	= V1 of (value1,value1) V.valueF
					| V1next of value2
					| V1mono of valueM
					
	and		value2	= V2 of (value2,value2) V.valueF
	and		valueM	= VM of (valueM,valueM) V.valueF
	and		  entry = Bind1 of value1 | Bind2 of value2 | Bind3 of valueM
	withtype   cont = entry MainDict.cont

	type t1 = value1 type t2 = value2 type t3 = valueM
	structure Base = BasicContext (MainDict) (type t = entry)
end
structure TC = ProjectTripleContext (Values)
open Values

fun holdGeneral (V1mono (VM (V.VFprim i))) = V1next (V2 (V.VFprim i))
  | holdGeneral _ = raise Stuck

fun V1unwrap (V1 v) = v
  | V1unwrap _ = raise Stuck
fun unnext (V1next v) = v
  | unnext _ = raise Stuck
fun unmono (V1mono v) = v
  | unmono _ = raise Stuck
  
structure Values1 = EmbedValues (struct
	type v = value1
	type f = value1
	val outof = V1unwrap
	val into = V1
end)
structure Values2 = EmbedValues (struct
	type v = value2
	type f = value2
	fun outof (V2 v) = v
	val into = V2
end)
structure ValuesM = EmbedValues (struct
	type v = valueM
	type f = valueM
	fun outof (VM v) = v
	val into = VM
end)

structure Evaluator1 = Evaluator (type t = value1) (Values1)
structure Evaluator2 = Evaluator (type t = value2) (Values2)
structure EvaluatorM = Evaluator (type t = valueM) (ValuesM)

fun ext1 g (P p) t = foldPattern (TC.C1.extend, ext1, Values1.untuple, Stuck) g p t
  | ext1 g (Pmono p) t = ext3 g p (unmono t)
  | ext1 g (Pnext p) t = ext2 g p (unnext t)
and ext2 g (PM p) t = foldPattern (TC.C2.extend, ext2, Values2.untuple, Stuck) g p t
and ext3 g (PM p) t = foldPattern (TC.C3.extend, ext3, ValuesM.untuple, Stuck) g p t

fun eval1 env (E1 exp) = Evaluator1.evalF env eval1 (ext1,TC.C1.lookup) exp
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
		
and evalM env (EM exp) = EvaluatorM.evalF env evalM (ext3,TC.C3.lookup) exp
and eval2 env (E2 exp) = Evaluator2.evalF env eval2 (ext2,TC.C2.lookup) exp
  | eval2 env (E2prev e) = unnext (eval1 env e)

end
end
