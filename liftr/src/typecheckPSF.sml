
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
fun tyAssertSame (SOME a, SOME b) = if teq a b then SOME a else raise TypeError
  | tyAssertSame (a, NONE) = a
  | tyAssertSame (NONE, b) = b

fun unfunc (SOME (Tfunc ab)) = ab
  | unfunc _ = raise TypeError
fun unprod (SOME (Tprod ab)) = ab
  | unprod _ = raise TypeError
fun unsum (SOME (Tsum ab)) = ab
  | unsum _ = raise TypeError

fun typeCheckS gamma exp = 
	let
		val check : expr -> ty option = typeCheckS gamma
		fun checkbranch t (v,e) = typeCheckS (extendContext gamma v t) e
	in
		case exp of 
		  Evar v => SOME (lookup gamma v)
		| Elam (t,b) => SOME (Tfunc (t, valOf (checkbranch t b)))
		| Eapp (e1,e2) => SOME (checkFun teq (unfunc (check e1), valOf (check e2)))
		| Etuple es => SOME (Tprod (map (valOf o check) es))
		| Epi (i, e) => SOME (List.nth (unprod (check e), i))
		| Einj (lr, t, e) => SOME (Tsum (injLR lr (valOf (check e)) t))
		| Ecase (e1,b1,b2) => tyAssertSame (zip2 checkbranch (unsum (check e1)) (b1,b2))
		| Elet (e, b) => checkbranch (valOf (check e)) b
		| Eerror => NONE
	end

fun typeCheck exp = 
	case typeCheckS empty exp of
	  NONE => raise TypeError
	| SOME t => t
	
end
