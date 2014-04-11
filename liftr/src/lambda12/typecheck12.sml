
structure Typecheck12 = 
struct

open LangCommon
open SourceLang
open Lambda12

fun t2eq (T2 t1) (T2 t2) = teq t2eq t1 t2
fun t1eq (T1 t1) (T1 t2) = teq t1eq t1 t2
  | t1eq (T1fut t) (T1fut u) = (t2eq t u)
  | t1eq _ _ = false
  
fun Tunwrap (T1 t) = t
  | Tunwrap _ = raise TypeError
fun unfut (T1fut t) = t
  | unfut _ = raise TypeError

structure Prim1 = Prims.PrimTyper (struct type t = type1 val Tint = T1 TFint val Tbool = T1 TFbool end)
structure Prim2 = Prims.PrimTyper (struct type t = type2 val Tint = T2 TFint val Tbool = T2 TFbool end)

fun typeCheck1 gamma (E1 exp) = typeCheck gamma typeCheck1 extendLookup1 T1 Tunwrap t1eq Prim1.getTypes exp
  | typeCheck1 gamma (E1next e) = T1fut (typeCheck2 gamma e)
  | typeCheck1 gamma (E1hold e) = (assertSame t1eq (T1 TFint, typeCheck1 gamma e); T1fut (T2 TFint))
	
and typeCheck2 gamma (E2 exp) = typeCheck gamma typeCheck2 extendLookup2 T2 (fn (T2 t) => t) t2eq Prim2.getTypes exp
  | typeCheck2 gamma (E2prev e) = unfut (typeCheck1 gamma e)

end
