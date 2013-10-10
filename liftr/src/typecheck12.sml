
structure Typecheck12 = 
struct

open LangCommon
open Lambda12

exception TypeError

fun t1eq T1unit T1unit = true
  | t1eq (T1prod (t1,t2)) (T1prod (u1,u2)) = (t1eq t1 u1) andalso (t1eq t2 u2)
  | t1eq (T1sum (t1,t2)) (T1sum (u1,u2)) = (t1eq t1 u1) andalso (t1eq t2 u2)
  | t1eq (T1func (t1,t2)) (T1func (u1,u2)) = (t1eq t1 u1) andalso (t1eq t2 u2)
  | t1eq (T1fut t) (T1fut u) = (t2eq t u)
  | t1eq _ _ = false
  
and t2eq T2unit T2unit = true
  | t2eq (T2prod (t1,t2)) (T2prod (u1,u2)) = (t2eq t1 u1) andalso (t2eq t2 u2)
  | t2eq (T2sum  (t1,t2)) (T2sum  (u1,u2)) = (t2eq t1 u1) andalso (t2eq t2 u2)
  | t2eq (T2func (t1,t2)) (T2func (u1,u2)) = (t2eq t1 u1) andalso (t2eq t2 u2)
  | t2eq _ _ = false

fun t1assertSame a b = if t1eq a b then a else raise TypeError
  
fun typeCheck1 gamma exp = 
	case exp of 
	  E1var (v) => (
		case lookup gamma v
		Stage1 ty => ty
		Stage2 _ => raise typeError)
	| E1lam (v,t,e) => Tfunc (t, typeCheck1 (extendContext gamma v (Stage1 t)) e1)
	| E1app (e1,e2) => (
		case (typeCheck1 gamma e1, typeCheck1 gamma e2) of
		  (T1func (a,b), c) => if ty1eq a c then b else raise TypeError
		| _ => raise TypeError )
	| E1unit => T1unit
	| E1tuple (e1,e2) => T1prod (typeCheck1 gamma e1, typeCheck1 gamma e2)
	| E1pi (lr, e) => (
		case (lr, typeCheck1 gamma e) of
		  (Left, T1prod (t,_)) => t
		  (Right, T1prod (_,t)) => t
		| _ => raise TypeError )
	| E1inj (lr, t, e) => (
		case (lr, typeCheck1 gamma e) of
		  (Left, tl) => T1Sum (tl, t)
		  (Right, tr) => T1Sum (t, tr)
		| _ => raise TypeError )
	| E1case (e1,(v2,e2),(v3,e3)) => 
		case typeCheck1 gamma e1 of
		  T1sum (a,b) => t1assertSame (
						typeCheck1 (extendContext gamma v2 (Stage1 a)) e2, 
						typeCheck1 (extendContext gamma v2 (Stage1 b)) e2)
		| _ => raise TypeError
	| E1next e => T1fut (typeCheck2 gamma e)
	
and typeCheck2 gamma exp = 
	case exp of 
	  E1var (v) => (
		case lookup gamma v
		Stage1 _ => raise typeError
		Stage2 ty => ty)
	| E1lam (v,t,e) => Tfunc (t, typeCheck2 (extendContext gamma v (Stage1 t)) e1)
	| E1app (e1,e2) => (
		case (typeCheck2 gamma e1, typeCheck2 gamma e2) of
		  (T1func (a,b), c) => if ty1eq a c then b else raise TypeError
		| _ => raise TypeError )
	| E1unit => T1unit
	| E1tuple (e1,e2) => T1prod (typeCheck2 gamma e1, typeCheck2 gamma e2)
	| E1pi (lr, e) => (
		case (lr, typeCheck2 gamma e) of
		  (Left, T1prod (t,_)) => t
		  (Right, T1prod (_,t)) => t
		| _ => raise TypeError )
	| E1inj (lr, t, e) => (
		case (lr, typeCheck2 gamma e) of
		  (Left, tl) => T1Sum (tl, t)
		  (Right, tr) => T1Sum (t, tr)
		| _ => raise TypeError )
	| E1case (e1,(v2,e2),(v3,e3)) => 
		case typeCheck2 gamma e1 of
		  T1sum (a,b) => t1assertSame (
						typeCheck2 (extendContext gamma v2 (Stage1 a)) e2, 
						typeCheck2 (extendContext gamma v2 (Stage1 b)) e2)
		| _ => raise TypeError
	| E2prev e => (
		case typeCheck1 gamma e of
		  T1fut t => t
		| _ => raise TypeError)		
	| E2save e => transfer (typeCheck2 gamma e)

and transfer T1unit = T1unit
  | transfer (T1prod (t1,t2)) = T2prod (transfer t1, transfer t2)
  | transfer (T1sum  (t1,t2)) = T2sum  (transfer t1, transfer t2)
  | transfer (T1func (t1,t2)) = raise TypeError
  | transfer (T1fut t) = raise TypeError

end
