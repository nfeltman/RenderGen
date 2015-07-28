
structure ErasureSemantics = 
struct

local
open LangCommon
open Lambda12
open SourceLang
structure V = ValuesBase
in

datatype value	= V1 of (value,value) V.valueF
				| V1next of value
				| V1mono of value

structure Values = BasicContext (MainDict) (type t = value)

fun holdGeneral (V1mono (V1 (V.VFprim i))) = V1next (V1 (V.VFprim i))
  | holdGeneral _ = raise Stuck

fun V1unwrap (V1 v) = v
  | V1unwrap _ = raise Stuck
fun unnext (V1next v) = v
  | unnext _ = raise Stuck
fun unmono (V1mono v) = v
  | unmono _ = raise Stuck
  
structure Values1 = EmbedValues (struct
	type v = value
	type f = value
	val outof = V1unwrap
	val into = V1
end)

structure Evaluator1 = Evaluator (type t = value) (Values1)

fun ext g (P p) t = foldPattern (Values.extend, ext, Values1.untuple, Stuck) g p t
  | ext g (Pmono p) t = ext g p (unmono t)
  | ext g (Pnext p) t = ext g p (unnext t)

fun eval env (L12core exp) = Evaluator1.evalF env eval (ext,Values.lookup) exp
  | eval env (L12stage exp) = 
  	case exp of
	  E1next e => V1next (eval env e)
  	| E1hold e => holdGeneral (eval env e)
  	| E1mono e => V1mono (eval env e)
  	| E1pushPrim e => Values1.makeprim (Values1.unprim (unmono (eval env e)))
  	| E1pushSum e => 
		let 
			val (i,v) = Values1.uninj (unmono (eval env e))
		in
			Values1.makeinj (i, V1mono v)
		end
  	| E2prev e => unnext (eval env e)

end
end
