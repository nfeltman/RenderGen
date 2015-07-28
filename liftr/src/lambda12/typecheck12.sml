
structure Typecheck12 = 
struct

local

infixr 9 `
fun a ` b = a b

open LangCommon
open SourceLang
open Lambda12
open TypesBase

fun t1eq (Tcore t1) (Tcore t2) = teq t1eq t1 t2
  | t1eq (Tfut t) (Tfut u) = (t1eq t u)
  | t1eq (Tnow t) (Tnow u) = (t1eq t u)
  | t1eq _ _ = false

fun lift1 n (Tcore t) = Tcore (TypeSubst.liftTy lift1 n t)
  | lift1 n (Tfut t) = Tfut (lift1 n t)
  | lift1 n (Tnow t) = Tnow (lift1 n t)

fun subst1 n (v) (Tcore t) = TypeSubst.substTy subst1 lift1 Tcore n v t
  | subst1 n (_) (Tfut t) = Tfut t
  | subst1 n (_) (Tnow t) = Tnow t

structure TS1 : TypeSystem = 
struct
	type ty = type12
	val teq = t1eq
	fun toString t = "????"
end

fun mapboth f (a,b) = (f a, f b)


in
	
fun unfut (Tfut t) = t
  | unfut _ = raise (TypeError "expected $")
fun unnow (Tnow t) = t
  | unnow _ = raise (TypeError "expected ^")

structure TypeFeatures1 = EmbedTypes(struct 
	type u = type12
	val into = Tcore
	fun outof (Tcore t) = t
	  | outof _ = raise (TypeError "expected core type")
	val subst = subst1 0
end)

structure MyContext = BasicContext (MainDict) (type t=type12)

structure Pattern1 = 
struct
	type p = pattern12
	type c = MyContext.cont
	type t = type12
	fun fold g (P p) v = foldPattern (MyContext.extend, fold, TypeFeatures1.unprod, TypeError "pattern") g p v
	  | fold g (Pmono p) v = fold g p (unnow v)
	  | fold g (Pnext p) v = fold g p (unfut v)
end


fun handleHold (Tnow (Tcore (TFprim t))) = Tfut (Tcore (TFprim t))
  | handleHold _ = raise (TypeError "expected ^prim")

structure Checker1 = TypeChecker (TS1) (TypeFeatures1) (MyContext) (Pattern1)

fun typeCheckM gamma (L12core e) = Checker1.typeCheck gamma typeCheckM e
  | typeCheckM gamma (L12stage _) = raise (TypeError "world violation: staging feature at 1g")

fun typeCheck1 gamma (L12core exp) = Checker1.typeCheck gamma typeCheck1 exp
  | typeCheck1 gamma (L12stage exp) = 
  	case exp of
	  (E1next e) => Tfut (typeCheck2 gamma e)
	| (E1hold e) => handleHold (typeCheck1 gamma e)
	| (E1mono e) => Tnow (typeCheckM gamma e)
	| (E1pushPrim e) => TypeFeatures1.makeprim ` TypeFeatures1.unprim ` unnow ` typeCheck1 gamma e
	| (E1pushSum e) => TypeFeatures1.makesum ` map Tnow ` TypeFeatures1.unsum ` unnow ` typeCheck1 gamma e
	| (E2prev _) => raise (TypeError "world violation: prev at 1M")
	
and typeCheck2 gamma (L12core exp) = Checker1.typeCheck gamma typeCheck2 exp
  | typeCheck2 gamma (L12stage (E2prev e)) = unfut (typeCheck1 gamma e)
  | typeCheck2 gamma (L12stage _) = raise (TypeError "world violation: staging feature at 2")
  
val emptyContext = MyContext.empty

end
end
