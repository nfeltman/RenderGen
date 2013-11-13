
structure Typecheck12 = 
struct

open LangCommon
open Lambda12

exception TypeError

fun t1eq T1unit T1unit = true
  | t1eq (T1prod (t1,t2)) (T1prod (u1,u2)) = (t1eq t1 u1) andalso (t1eq t2 u2)
  | t1eq (T1sum (t1,t2))  (T1sum (u1,u2))  = (t1eq t1 u1) andalso (t1eq t2 u2)
(*  | t1eq (T1func (t1,t2)) (T1func (u1,u2)) = (t1eq t1 u1) andalso (t1eq t2 u2)*)
  | t1eq (T1fut t) (T1fut u) = (t2eq t u)
  | t1eq _ _ = false
  
and t2eq T2unit T2unit = true
  | t2eq (T2prod (t1,t2)) (T2prod (u1,u2)) = (t2eq t1 u1) andalso (t2eq t2 u2)
  | t2eq (T2sum  (t1,t2)) (T2sum  (u1,u2)) = (t2eq t1 u1) andalso (t2eq t2 u2)
(*  | t2eq (T2func (t1,t2)) (T2func (u1,u2)) = (t2eq t1 u1) andalso (t2eq t2 u2)*)
  | t2eq _ _ = false

fun checkFun eq ((a,b),c) = if eq a c then b else raise TypeError
fun t1assertSame (a,b) = if t1eq a b then a else raise TypeError
fun t2assertSame (a,b) = if t2eq a b then a else raise TypeError

fun unstage1 (Stage1 t) = t
  | unstage1 _ = raise TypeError
fun unstage2 (Stage2 t) = t
  | unstage2 _ = raise TypeError
fun unfunc1 (Func1 ab) = ab
  | unfunc1 _ = raise TypeError
fun unfunc2 (Func2 ab) = ab
  | unfunc2 _ = raise TypeError
fun unprod1 (T1prod ab) = ab
  | unprod1 _ = raise TypeError
fun unsum1 (T1sum ab) = ab
  | unsum1 _ = raise TypeError
(*fun unfun1 (T1sum ab) = ab
  | unfun1 _ = raise TypeError*)
fun unfut (T1fut t) = t
  | unfut _ = raise TypeError
fun unprod2 (T2prod ab) = ab
  | unprod2 _ = raise TypeError
fun unsum2 (T2sum ab) = ab
  | unsum2 _ = raise TypeError
(*fun unfun2 (T2sum ab) = ab
  | unfun2 _ = raise TypeError*)

fun typeCheck1 gamma exp = 
	let
		val check = typeCheck1 gamma
		fun checkbranch t (v,e) = typeCheck1 (extendContext gamma v (Stage1 t)) e
	in
		case exp of 
		  E1var v => unstage1 (lookup gamma v)
	(*	| E1lam (t,b) => T1func (t, checkbranch t b)
		| E1app (e1,e2) => checkFun t1eq (unfun1 (check e1), check e2)*)
		| E1call (f, e) => checkFun t1eq (unfunc1 (lookup gamma f), check e)
		| E1unit => T1unit
		| E1tuple (e1,e2) => T1prod (check e1, check e2)
		| E1pi (lr, e) => projLR lr (unprod1 (check e)) 
		| E1inj (lr, t, e) => T1sum (injLR lr (check e) t)
		| E1case (e1,b1,b2) => t1assertSame (zip2 checkbranch (unsum1 (check e1)) (b1,b2))
		| E1next e => T1fut (typeCheck2 gamma e)
	end
	
and typeCheck2 gamma exp = 
	let
		val check = typeCheck2 gamma
		fun checkbranch t (v,e) = typeCheck2 (extendContext gamma v (Stage2 t)) e
	in
		case exp of 
		  E2var v => unstage2 (lookup gamma v)
	(*	| E2lam (t,b) => T2func (t, checkbranch t b)
		| E2app (e1,e2) => checkFun t2eq (unfun2 (check e1), check e2)*)
		| E2call (f, e) => checkFun t2eq (unfunc2 (lookup gamma f), check e)
		| E2unit => T2unit
		| E2tuple (e1,e2) => T2prod (check e1, check e2)
		| E2pi (lr, e) => projLR lr (unprod2 (check e)) 
		| E2inj (lr, t, e) => T2sum (injLR lr (check e) t)
		| E2case (e1,b1,b2) => t2assertSame (zip2 checkbranch (unsum2 (check e1)) (b1,b2))
		| E2prev e => unfut (typeCheck1 gamma e)
	end
	
fun checkProgram p = 
	let		
		fun checkFunc _ [] = ()
		  | checkFunc g (FuncDec1(f,t1,t2,v,e) :: fs) = 
				(t1assertSame (t2, typeCheck1 (extendContext g v (Stage1 t1)) e); 
				checkFunc (extendContext g f (Func1 (t1,t2))) fs)
		  | checkFunc g (FuncDec2(f,t1,t2,v,e) :: fs) = 
				(t2assertSame (t2, typeCheck2 (extendContext g v (Stage2 t1)) e); 
				checkFunc (extendContext g f (Func2 (t1,t2))) fs)
	in
		checkFunc empty p
	end

end
