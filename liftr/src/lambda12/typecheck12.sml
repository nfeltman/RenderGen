
structure Typecheck12 = 
struct

local
open LangCommon
open SourceLang
open Lambda12
open TypesBase
open Contexts

fun Tunwrap (T1 t) = t
  | Tunwrap _ = raise (TypeError "expected non-$")
fun unfut (T1fut t) = t
  | unfut _ = raise (TypeError "expected $")

fun t2eq (T2 t1) (T2 t2) = teq t2eq t1 t2
fun t1eq (T1 t1) (T1 t2) = teq t1eq t1 t2
  | t1eq (T1fut t) (T1fut u) = (t2eq t u)
  | t1eq _ _ = false

fun lift2 n (T2 t) = T2 (TypeSubst.liftTy lift2 n t)
fun lift1 n (T1 t) = T1 (TypeSubst.liftTy lift1 n t)
  | lift1 n (T1fut t) = T1fut (lift2 n t)

fun subst2 n (v : type2) (T2 t) = TypeSubst.substTy subst2 lift2 T2 n v t
fun subst1 n (v : type1) (T1 t) = TypeSubst.substTy subst1 lift1 T1 n v t
  | subst1 n (_ : type1) (T1fut t) = T1fut t

fun handleHold (T1 (TFprim t)) = T1fut (T2 (TFprim t))
  | handleHold _ = raise (TypeError "expected any primitive type")

structure TS1 : TypeSystem = 
struct
	type ty = type1
	val teq = t1eq
	fun toString t = "????"
end
structure TS2 : TypeSystem = 
struct
	type ty = type2
	val teq = t2eq
	fun toString t = "????"
end

structure Checker1 = TypeChecker (TS1)
structure Checker2 = TypeChecker (TS2)

in

fun typeCheck1 gamma (E1 exp) = Checker1.typeCheck gamma typeCheck1 extendLookup1 T1 Tunwrap subst1 exp
  | typeCheck1 gamma (E1next e) = T1fut (typeCheck2 gamma e)
  | typeCheck1 gamma (E1hold e) = handleHold (typeCheck1 gamma e)
	
and typeCheck2 gamma (E2 exp) = Checker2.typeCheck gamma typeCheck2 extendLookup2 T2 (fn (T2 t) => t) subst2 exp
  | typeCheck2 gamma (E2prev e) = unfut (typeCheck1 gamma e)

end
end
