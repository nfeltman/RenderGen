
structure TypecheckPSF = 
struct

structure Prim = Prims.PrimTyper (struct type t = LambdaPSF.ty val Tint = LambdaPSF.Tint val Tbool = LambdaPSF.Tbool end)

open LangCommon
open LambdaPSF

exception TypeError

fun teq Tint Tint = true
  | teq Tbool Tbool = true
  | teq (Tprod ts1) (Tprod ts2) = listeq teq ts1 ts2
  | teq (Tsum  (t1,t2)) (Tsum  (u1,u2)) = (teq t1 u1) andalso (teq t2 u2)
  | teq (Tfunc (t1,t2)) (Tfunc (u1,u2)) = (teq t1 u1) andalso (teq t2 u2)
  | teq _ _ = false

fun checkFun eq ((a,b),c) = if eq a c then b else raise TypeError
fun tyAssertSame (a, b) = if teq a b then a else raise TypeError
fun binSame eq (a,b) (c,d,e) = if (eq a c) andalso (eq b d) then e else raise TypeError

fun unfunc (Tfunc ab) = ab
  | unfunc _ = raise TypeError
fun unprod (Tprod ab) = ab
  | unprod _ = raise TypeError
fun unsum (Tsum ab) = ab
  | unsum _ = raise TypeError

fun typeCheckS gamma exp = 
	let
		val check = typeCheckS gamma
		fun checkbranch t (v,e) = typeCheckS (extendContext gamma v t) e
	in
		case exp of 
		  Evar v => lookup gamma v
		| Elam (t,b) => Tfunc (t, checkbranch t b)
		| Eapp (e1,e2) => checkFun teq (unfunc (check e1), check e2)
		| Etuple es => Tprod (map check es)
		| Epi (i, e) => List.nth (unprod (check e), i)
		| Einj (lr, t, e) => Tsum (injLR lr (check e) t)
		| Ecase (e1,b1,b2) => tyAssertSame (zip2 checkbranch (unsum (check e1)) (b1,b2))
		| Elet (e, b) => checkbranch (check e) b
		| Ebinop (bo, e1, e2) => binSame teq (check e1, check e2) (Prim.getTypes bo)
		| Eroll e => raise TypeError
		| Eunroll e => raise TypeError
		| Eerror t => t
	end

fun typeCheck exp = typeCheckS empty exp
	
end
