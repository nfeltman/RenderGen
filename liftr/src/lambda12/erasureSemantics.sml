
structure ErasureSemantics = 
struct

local
open LangCommon
open Lambda12
open SourceLang
structure V = ValuesBase
in

datatype valueMono	= VM of (valueMono,valueMono) V.valueF
datatype value1	= V1 of (value1,value1) V.valueF
				| V1next of valueMono
				| V1mono of valueMono

structure Values = TripleContext (MainDict) (type t1 = value1) (type t2 = valueMono) (type t3 = valueMono)
structure TC = ProjectTripleContext (Values)

fun holdGeneral (V1mono (VM (V.VFprim i))) = V1next (VM (V.VFprim i))
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
structure ValuesMono = EmbedValues (struct
	type v = valueMono
	type f = valueMono
	fun outof (VM v) = v
	val into = VM
end)

structure Evaluator1 = Evaluator (type t = value1) (Values1)
structure EvaluatorM = Evaluator (type t = valueMono) (ValuesMono)

fun ext1 g (P p) t = foldPattern (TC.C1.extend, ext1, Values1.untuple, Stuck) g p t
  | ext1 g (Pmono p) t = ext3 g p (unmono t)
  | ext1 g (Pnext p) t = ext2 g p (unnext t)
and ext2 g (PM p) t = foldPattern (TC.C2.extend, ext2, ValuesMono.untuple, Stuck) g p t
and ext3 g (PM p) t = foldPattern (TC.C3.extend, ext3, ValuesMono.untuple, Stuck) g p t

fun eval1 env (E1 exp) = Evaluator1.evalF env eval1 (ext1,TC.C1.lookup) exp
  | eval1 env (E1next e) = V1next (eval2 env e)
  | eval1 env (E1hold e) = holdGeneral (eval1 env e)
  | eval1 env (E1mono e) = V1mono (evalM env e)
  | eval1 env (E1pushPrim e) = Values1.makeprim (ValuesMono.unprim (unmono (eval1 env e)))
  | eval1 env (E1pushProd e) = Values1.maketuple (map V1mono (ValuesMono.untuple (unmono (eval1 env e))))
  | eval1 env (E1pushSum e) = 
		let 
			val (i,v) = ValuesMono.uninj (unmono (eval1 env e))
		in
			Values1.makeinj (i, V1mono v)
		end
		
and evalM env (EM exp) = EvaluatorM.evalF env evalM (ext3,TC.C3.lookup) exp
and eval2 env (E2 exp) = EvaluatorM.evalF env eval2 (ext2,TC.C2.lookup) exp
  | eval2 env (E2prev e) = unnext (eval1 env e)

end
end
