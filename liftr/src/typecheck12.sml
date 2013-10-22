
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
fun t2assertSame a b = if t2eq a b then a else raise TypeError
  
fun typeCheck1 gamma exp = 
	case exp of 
	  E1var (v) => (
		case lookup gamma v of
		  Stage1 ty => ty
		| Stage2 _ => raise TypeError)
	| E1lam (v,t,e) => T1func (t, typeCheck1 (extendContext gamma v (Stage1 t)) e)
	| E1app (e1,e2) => (
		case (typeCheck1 gamma e1, typeCheck1 gamma e2) of
		  (T1func (a,b), c) => if t1eq a c then b else raise TypeError
		| _ => raise TypeError )
	| E1unit => T1unit
	| E1tuple (e1,e2) => T1prod (typeCheck1 gamma e1, typeCheck1 gamma e2)
	| E1pi (lr, e) => (
		case (lr, typeCheck1 gamma e) of
		  (Left, T1prod (t,_)) => t
		| (Right, T1prod (_,t)) => t
		| _ => raise TypeError )
	| E1inj (lr, t, e) => (
		case (lr, typeCheck1 gamma e) of
		  (Left, tl) => T1sum (tl, t)
		| (Right, tr) => T1sum (t, tr))
	| E1case (e1,(v2,e2),(v3,e3)) => (
		case typeCheck1 gamma e1 of
		  T1sum (a,b) => t1assertSame 
						(typeCheck1 (extendContext gamma v2 (Stage1 a)) e2)
						(typeCheck1 (extendContext gamma v2 (Stage1 b)) e2)
		| _ => raise TypeError)
	| E1next e => T1fut (typeCheck2 gamma e)
	
and typeCheck2 gamma exp = 
	case exp of 
	  E2var (v) => (
		case lookup gamma v of
		  Stage1 _ => raise TypeError
		| Stage2 ty => ty)
	| E2lam (v,t,e) => T2func (t, typeCheck2 (extendContext gamma v (Stage2 t)) e)
	| E2app (e1,e2) => (
		case (typeCheck2 gamma e1, typeCheck2 gamma e2) of
		  (T2func (a,b), c) => if t2eq a c then b else raise TypeError
		| _ => raise TypeError )
	| E2unit => T2unit
	| E2tuple (e1,e2) => T2prod (typeCheck2 gamma e1, typeCheck2 gamma e2)
	| E2pi (lr, e) => (
		case (lr, typeCheck2 gamma e) of
		  (Left, T2prod (t,_)) => t
		| (Right, T2prod (_,t)) => t
		| _ => raise TypeError )
	| E2inj (lr, t, e) => (
		case (lr, typeCheck2 gamma e) of
		  (Left, tl) => T2sum (tl, t)
		| (Right, tr) => T2sum (t, tr) )
	| E2case (e1,(v2,e2),(v3,e3)) => (
		case typeCheck2 gamma e1 of
		  T2sum (a,b) => t2assertSame 
						(typeCheck2 (extendContext gamma v2 (Stage2 a)) e2)
						(typeCheck2 (extendContext gamma v2 (Stage2 b)) e2)
		| _ => raise TypeError)
	| E2prev e => (
		case typeCheck1 gamma e of
		  T1fut t => t
		| _ => raise TypeError)

and transfer T1unit = T2unit
  | transfer (T1prod (t1,t2)) = T2prod (transfer t1, transfer t2)
  | transfer (T1sum  (t1,t2)) = T2sum  (transfer t1, transfer t2)
  | transfer (T1func (t1,t2)) = raise TypeError
  | transfer (T1fut t) = raise TypeError

end
