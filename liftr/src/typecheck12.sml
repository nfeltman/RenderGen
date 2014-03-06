
structure Typecheck12 = 
struct

open LangCommon
open Lambda12

exception TypeError

fun teq _ TFint TFint = true
  | teq _ TFbool TFbool = true
  | teq _ TFunit TFunit = true
  | teq eq (TFprod (t1,t2)) (TFprod (u1,u2)) = (eq t1 u1) andalso (eq t2 u2)
  | teq _ _ _ = false

fun t2eq (T2 t1) (T2 t2) = teq t2eq t1 t2
fun t1eq (T1 t1) (T1 t2) = teq t1eq t1 t2
  | t1eq (T1fut t) (T1fut u) = (t2eq t u)
  | t1eq _ _ = false
  

fun unstage1 (Stage1 t) = t
  | unstage1 _ = raise TypeError
fun unstage2 (Stage2 t) = t
  | unstage2 _ = raise TypeError
fun unprodF (TFprod ab) = ab
  | unprodF _ = raise TypeError
fun unfut (T1fut t) = t
  | unfut _ = raise TypeError

fun checkFun eq ((a,b),c) = if eq a c then b else raise TypeError
fun assertSame eq (a,b) = if eq a b then a else raise TypeError
fun binSame eq (a,b) (c,d,e) = if (eq a c) andalso (eq b d) then e else raise TypeError

structure Prim1 = Prims.PrimTyper (struct type t = type1 val Tint = T1 TFint val Tbool = T1 TFbool end)
structure Prim2 = Prims.PrimTyper (struct type t = type2 val Tint = T2 TFint val Tbool = T2 TFbool end)

fun typeCheck gamma unstage checkrec stage Twrap Tunwrap teq getTypes exp = 
	let
		val check = checkrec gamma
		fun checkbranch t (v,e) = checkrec (extendContext gamma v (stage t)) e
	in
		case exp of 
		  Fvar v => unstage (lookup gamma v)
	(*	| Flam (t,b) => T1func (t, checkbranch t b)
		| Fapp (e1,e2) => checkFun t1eq (unfun1 (check e1), check e2)*)
	(*	| Fcall (f, e) => checkFun t1eq (unfunc1 (lookup gamma f), check e) *)
		| Funit => Twrap TFunit
		| Fint _ => Twrap TFint
		| Fbool _ => Twrap TFbool
		| Ftuple (e1,e2) => Twrap (TFprod (check e1, check e2))
		| Fpi (lr, e) => projLR lr (unprodF (Tunwrap (check e)))
	(*	| Finj (lr, t, e) => T1sum (injLR lr (check e) t)
		| Fcase (e1,b1,b2) => assertSame t1eq (zip2 checkbranch (unsum1 (check e1)) (b1,b2)) *)
		| Fif (e1,e2,e3) => (assertSame teq (Twrap TFbool, check e1); assertSame teq (check e2, check e3))
		| Flet (e,b) => checkbranch (check e) b
		| Ferror t => t
		| Fbinop (bo, e1, e2) => binSame teq (check e1, check e2) (getTypes bo)
	end

fun typeCheck1 gamma (E1 exp) = 
	let
		fun Tunwrap (T1 t) = t
		  | Tunwrap _ = raise TypeError
	in
		typeCheck gamma unstage1 typeCheck1 Stage1 T1 Tunwrap t1eq Prim1.getTypes exp
	end
  | typeCheck1 gamma (E1next e) = T1fut (typeCheck2 gamma e)
  | typeCheck1 gamma (E1hold e) = (assertSame t1eq (T1 TFint, typeCheck1 gamma e); T1fut (T2 TFint))
	
and typeCheck2 gamma (E2 exp) = 
	let
		fun Tunwrap (T2 t) = t
	in
		typeCheck gamma unstage2 typeCheck2 Stage2 T2 Tunwrap t2eq Prim2.getTypes exp
	end
  | typeCheck2 gamma (E2prev e) = unfut (typeCheck1 gamma e)
	
	(*
fun checkProgram p = 
	let		
		fun checkFunc _ [] = ()
		  | checkFunc g (FuncDec1(f,t1,t2,v,e) :: fs) = 
				(assertSame t1eq (t2, typeCheck1 (extendContext g v (Stage1 t1)) e); 
				checkFunc (extendContext g f (Func1 (t1,t2))) fs)
	(*	  | checkFunc g (FuncDec2(f,t1,t2,v,e) :: fs) = 
				(t2assertSame (t2, typeCheck2 (extendContext g v (Stage2 t1)) e); 
				checkFunc (extendContext g f (Func2 (t1,t2))) fs) *)
	in
		checkFunc empty p
	end*)

end
