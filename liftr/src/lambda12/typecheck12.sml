
structure Typecheck12 = 
struct

local

infixr 9 `
fun a ` b = a b

open LangCommon
open SourceLang
open Lambda12
open TypesBase

fun Tunwrap (T1 t) = t
  | Tunwrap _ = raise (TypeError "expected non-stage type")

fun t2eq (T2 t1) (T2 t2) = teq t2eq t1 t2
fun t1eq (T1 t1) (T1 t2) = teq t1eq t1 t2
  | t1eq (T1fut t) (T1fut u) = (t2eq t u)
  | t1eq (T1now t) (T1now u) = (t2eq t u)
  | t1eq _ _ = false

fun lift2 n (T2 t) = T2 (TypeSubst.liftTy lift2 n t)
fun lift1 n (T1 t) = T1 (TypeSubst.liftTy lift1 n t)
  | lift1 n (T1fut t) = T1fut (lift2 n t)
  | lift1 n (T1now t) = T1now (lift2 n t)

fun subst2 n (v : type2) (T2 t) = TypeSubst.substTy subst2 lift2 T2 n v t
fun subst1 n (v : type1) (T1 t) = TypeSubst.substTy subst1 lift1 T1 n v t
  | subst1 n (_ : type1) (T1fut t) = T1fut t
  | subst1 n (_ : type1) (T1now t) = T1now t

and noArrow (T2 (TFarr _)) = raise (TypeError "mono type bad")
  | noArrow (T2 (TFprod ts)) = List.app noArrow ts
  | noArrow (T2 (TFsum ts)) = List.app noArrow ts
  | noArrow (T2 (TFrec t)) = noArrow t
  | noArrow (T2 (TFvar _)) = ()
  | noArrow (T2 (TFprim _)) = ()
fun monoSafe (t as T2 (TFarr (t1,t2))) = (noArrow t1; monoSafe t2; t)
  | monoSafe t = (noArrow t; t)

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

fun mapboth f (a,b) = (f a, f b)


in
	
fun unfut (T1fut t) = t
  | unfut _ = raise (TypeError "expected $")
fun unnow (T1now t) = t
  | unnow _ = raise (TypeError "expected ^")

structure TypeFeatures1 = EmbedTypes(struct 
	type u = type1 
	val into = T1 
	val outof = Tunwrap 
	val subst = subst1 0
end)
structure TypeFeatures2 = EmbedTypes(struct 
	type u = type2 
	val into = T2 
	fun outof (T2 t) = t 
	val subst = subst2 0
end)

structure MyContext = TripleContext (ListDict (type var = var)) (type t1=type1) (type t2=type2) (type t3=type2)

structure Pattern3 = 
struct
	type p = patternM
	type c = MyContext.C3.cont
	type t = type2
	fun fold g (PM p) v = foldPattern (MyContext.C3.extend, fold, TypeFeatures2.unprod, TypeError "pattern") g p v
end
structure Pattern1 = 
struct
	type p = pattern12
	type c = MyContext.C1.cont
	type t = type1
	fun fold g (P p) v = foldPattern (MyContext.C1.extend, fold, TypeFeatures1.unprod, TypeError "pattern") g p v
	  | fold g (Pmono p) v = Pattern3.fold g p (unnow v)
end
structure Pattern2 = 
struct
	type p = patternM
	type c = MyContext.C2.cont
	type t = type2
	fun fold g (PM p) v = foldPattern (MyContext.C2.extend, fold, TypeFeatures2.unprod, TypeError "pattern") g p v
end


fun handleHold (T1 (TFprim t)) = T1fut (T2 (TFprim t))
  | handleHold _ = raise (TypeError "expected any primitive type")

fun promoteType (T2 t) = T1 (mapType promoteType t)

structure Checker1 = TypeChecker (TS1) (TypeFeatures1) (MyContext.C1) (Pattern1)
structure Checker2 = TypeChecker (TS2) (TypeFeatures2) (MyContext.C2) (Pattern2)
structure CheckerM = TypeChecker (TS2) (TypeFeatures2) (MyContext.C3) (Pattern3)

fun typeCheckM gamma (EM e) = CheckerM.typeCheck gamma typeCheckM e

fun typeCheck1 gamma (E1 exp) = Checker1.typeCheck gamma typeCheck1 exp
  | typeCheck1 gamma (E1next e) = T1fut (typeCheck2 gamma e)
  | typeCheck1 gamma (E1hold e) = handleHold (typeCheck1 gamma e)
  | typeCheck1 gamma (E1mono e) = T1now (typeCheckM gamma e)
  | typeCheck1 gamma (E1pushPrim e) = TypeFeatures1.makeprim ` TypeFeatures2.unprim ` unnow ` typeCheck1 gamma e
  | typeCheck1 gamma (E1pushProd e) = TypeFeatures1.makeprod ` map T1now ` TypeFeatures2.unprod ` unnow ` typeCheck1 gamma e
  | typeCheck1 gamma (E1pushSum e) = TypeFeatures1.makesum ` map T1now ` TypeFeatures2.unsum ` unnow ` typeCheck1 gamma e
  | typeCheck1 gamma (E1pushArr e) = TypeFeatures1.makearr ` mapboth T1now ` TypeFeatures2.unarr ` unnow ` typeCheck1 gamma e
	
and typeCheck2 gamma (E2 exp) = Checker2.typeCheck gamma typeCheck2 exp
  | typeCheck2 gamma (E2prev e) = unfut (typeCheck1 gamma e)
  
val emptyContext = MyContext.Base.empty

end
end
