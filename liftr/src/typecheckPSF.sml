
structure TypecheckPSF = 
struct

open LangCommon
open LambdaPSF

exception TypeError

fun teq (Tprod ts1) (Tprod ts2) = listeq teq ts1 ts2
  | teq (Tsum  (t1,t2)) (Tsum  (u1,u2)) = (teq t1 u1) andalso (teq t2 u2)
  | teq (Tfunc (t1,t2)) (Tfunc (u1,u2)) = (teq t1 u1) andalso (teq t2 u2)
  | teq _ _ = false

fun checkFun eq ((a,b),c) = if eq a c then b else raise TypeError
fun tyAssertSame (a, b) = if teq a b then a else raise TypeError

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
		| Eerror t => t
	end

fun typeCheck exp = typeCheckS empty exp
	
end
