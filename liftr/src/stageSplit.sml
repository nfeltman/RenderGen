
structure StageSplit = 
struct

open LangCommon
open LambdaPSF
open Typecheck12

fun trType2 T2unit = Tprod []
  | trType2 (T2prod (t1,t2)) = Tprod [trType2 t1, trType2 t2]
  | trType2 (T2sum (t1,t2)) = Tsum (trType2 t1, trType2 t2)

fun firstImage T1unit = Tprod []
  | firstImage (T1prod (t1,t2)) = Tprod [firstImage t1, firstImage t2]
  | firstImage (T1sum (t1,t2)) = Tsum (firstImage t1, firstImage t2)
  | firstImage (T1fut _) = Tprod []

fun secondImage T1unit = Tprod []
  | secondImage (T1prod (t1,t2)) = Tprod [firstImage t1, firstImage t2]
  | secondImage (T1sum (t1,t2)) = Tsum (firstImage t1, firstImage t2)
  | secondImage (T1fut t) = trType2 t

fun fresh () = Variable.newvar "l"
fun dummy () = Variable.newvar "dummy"

(* map precomp / residual *)
fun mapSnd f (a,b) = (a, f b)
fun mapp f (a,b,c) = (f a, b, c)

val Eunit = Etuple []
fun bind e b = Elet (e,b)
fun deadBranch () = (dummy (), Eerror)

fun freshPi () = 
	let
		val l = fresh ()
	in
		(l, fn i => Epi (i, Evar l))
	end

fun splitSubsBase split () = 
	let
		val (l,pi) = freshPi ()
		fun mapThird f (a,b,c) = (a, b, f c)
		fun splitBind e i = mapThird (bind (pi i)) (split e)
	in
		(l, splitBind)
	end
	
(* assume here that the expression already type-checks *)
fun stageSplit1 gamma exp = 
	let
		val split = stageSplit1 gamma
		val splitSubs = splitSubsBase split
		fun bindProj e f = 
			let
				val (v,pi) = freshPi ()
			in
				bind e (v, f (pi 0, pi 1))
			end
		fun id x = x
		fun bindMap e f g = bindProj e (fn (e1,e2) => Etuple[f e1, g e2])
	in
		case exp of 
		  E1var v => (Etuple [Evar v, Eunit], Tprod [], (dummy (), Evar v))
	(*	| E2lam (t,(x,e)) => 
			let
				val (c,lr) = split e
				val (y, pi) = freshPi ()
			in
				(
					Etuple [Elam (Tgap, (x, c)), Eunit], 
					(dummy (), Elam (Tunit, (y, bind (pi 0) (x, bind (pi 1) lr))))
				)
			end
		| E2app (e1, e2) => 
			let
				val (y, pi) = freshPi ()
				val (l,splitDecompBind) = splitSubs ()
				val (c11,c12,letc1,bound1) = splitDecompBind e1 0
				val (c21,c22,letc2,bound2) = splitDecompBind e2 1
			in
				(
					letc1 (letc2 (bind (Eapp (c11, c21)) (y, Etuple [pi 0, Etuple [c12,c22,pi 1]]))), 
					(l, Eapp (bound1, Etuple [bound2, Epi(2, Evar l)]))
				)
			end *)
		| E1call (f, e) => 
			let
				val (link,splitBind) = splitSubs ()
				val (c,t,bound) = splitBind e 0
			in
				(
					bindProj c (fn (argVal,argPre) =>
						bindProj (Eapp (Evar f, argVal)) (fn (resVal,resBoun) => 
							Etuple [resVal, Etuple [argPre,resBoun]]
					)), 
					Tprod[t, lookup gamma f],
					(link, Eapp (Evar f, Etuple [bound, Epi(1, Evar link)]))
				)
			end
		| E1unit => (Etuple [Eunit, Eunit], Tprod [], (dummy (), Eunit))
		| E1tuple (e1, e2) => 
			let
				val (link, splitBind) = splitSubs ()
				val (c1, t1, bound1) = splitBind e1 0
				val (c2, t2, bound2) = splitBind e2 1
			in
				(
					bindProj c1 (fn (val1,pre1) =>
						bindProj c2 (fn (val2,pre2) =>
							Etuple [Etuple[val1,val2], Etuple[pre1,pre2]]
					)), 
					Tprod[t1,t2], 
					(link, Etuple [bound1, bound2])
				)
			end
		| E1pi (side, e) => 
			let
				val (c, t, lr) = split e
				fun proj x = Epi (case side of Left => 0 | Right => 1, x)
			in
				(
					bindMap c proj id,
					t,
					mapSnd proj lr
				)
			end
		| E1inj (side, otherT, e) => 
			let
				val (c, t, lr) = split e
				fun inj t x = Einj (side, t, x)
			in
				(
					bindMap c (inj (firstImage otherT)) id,
					t,
					mapSnd (inj (secondImage otherT)) lr
				)
			end
		| E1case (e,(x1,e1),(x2,e2)) =>
			let
				val (link, pi) = freshPi ()
				val (c,  t,  lr) = split e
				val (c1, t1, lr1) = split e1
				val (c2, t2, lr2) = split e2
				fun inj side t x = Einj (side, t, x)
			in
				(
					bindProj c (fn (predVal,predPre) =>
						bindProj
						(Ecase(predVal, 
							(x1, bindMap c1 id (inj Left t2)), 
							(x2, bindMap c2 id (inj Right t1))
						))
						(fn (y1,y2) => Etuple [y1, Etuple [predPre,y2]])
					), 
					Tprod [t, Tsum (t1,t2)],
					(link,  
						Ecase (bind (pi 0) lr,
							(x1, Ecase (pi 1, lr1, deadBranch ())),
							(x2, Ecase (pi 1, deadBranch (), lr2))
						)
					)
				)
			end
		| E1next e => mapp (fn p => Etuple [Eunit, p]) (stageSplit2 gamma e)
	end
	
and stageSplit2 gamma exp = 
	let
		val split = stageSplit2 gamma
		fun splitBr (v,e) = (v, split e)
		val splitSubs = splitSubsBase split
		fun splitBin (e1, e2) operation = 
			let
				val (l,splitBind) = splitSubs ()
				val (p1,t1,bound1) = splitBind e1 0
				val (p2,t2,bound2) = splitBind e2 1
			in
				(Etuple [p1,p2], Tprod [t1,t2], (l, operation (bound1, bound2) ))
			end
		fun mapbr f (c,t,(l,r)) = (c,t,(l,f r))
	in
		case exp of 
		  E2var v => (Eunit, Tprod [], (dummy (), Evar v))
	(*	| E2lam (t,(x,e)) => mapbr (fn r => Elam (Tgap,(x,r))) (split e)
		| E2app e12 => splitBin e12 Eapp *)
	(*	| E2call (f, e) => mapbr (fn r => Eapp (Evar f, r)) (split e) *)
		| E2unit => (Eunit, Tprod [], (dummy (), Eunit))
		| E2tuple e12 => splitBin e12 (fn (a,b) => Etuple [a,b])
		| E2pi (lr, e) => mapbr (fn r => Epi (case lr of Left => 0 | Right => 1, r)) (split e)
		| E2inj (lr, t, e) => mapbr (fn r => Einj (lr, trType2 t, r)) (split e)
		| E2case (p,(x1,e1),(x2,e2)) => 
			let
				val (l, splitBind) = splitSubs ()
				val (pp, tp, boundP) = splitBind p  0
				val (p1, t1, bound1) = splitBind e1 1
				val (p2, t2, bound2) = splitBind e2 2
			in
				(Etuple [pp, p1, p2], Tprod [tp, t1, t2], (l, Ecase (boundP, (x1,bound1), (x2,bound2))))
			end
		| E2prev e => mapp (fn c => Epi (1, c)) (stageSplit1 gamma e)
	end

fun splitProg prog = 
	let
		fun splitFunc _ [] = (Eunit, Eunit)
		  | splitFunc g (FuncDec1(f,t1,_,v,e) :: rest) = 
			let
				val (c,boundary,(l,r)) = stageSplit1 g e
				val (rest1, rest2) = splitFunc (extendContext g f boundary) rest
				val firstFunc = bind (Elam (firstImage t1,(v,c))) (f,rest1)
				val (x,pi) = freshPi ()
				val secondFunc = bind (
							Elam (Tprod [secondImage t1, boundary], (x, 
								bind (pi 0) (v, 
								bind (pi 1) (l, r)))
							)) (f,rest2)
			in
				(firstFunc, secondFunc)
			end
	in
		splitFunc empty prog
	end
	
end
