
structure StageSplit = 
struct

open LangCommon
open LambdaPSF
open Typecheck12

exception NotImplemented

fun fresh () = "l"
fun dummy () = "dummy"

(* map precomp / residual *)
fun mapSnd f (a,b) = (a, f b)
fun mapp f (a,b,c) = (f a, b, c)

val Eunit = Etuple []
fun bind _ _ = raise NotImplemented
fun deadBranch () = (dummy (), raise NotImplemented)

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
fun stageSplit1 exp = 
	let
		val split = stageSplit1
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
					raise NotImplemented,
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
		| E1inj (side, _, e) => 
			let
				val (c, t, lr) = split e
				fun inj t x = Einj (side, t, x)
			in
				(
					bindMap c (inj Tgap) id,
					t,
					mapSnd (inj Tgap) lr
				)
			end
		| E1case (e,(x1,e1),(x2,e2)) =>
			let
				val (link, pi) = freshPi ()
				val (c,  t,  lr) = split e
				val (c1, t1, lr1) = split e1
				val (c2, t2, lr2) = split e2
				fun inj side x = Einj (side, Tgap, x)
			in
				(
					bindProj c (fn (predVal,predPre) =>
						bindProj
						(Ecase(predVal, 
							(x1, bindMap e1 id (inj Left)), 
							(x2, bindMap e2 id (inj Right))
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
		| E1next e => mapp (fn p => Etuple [Eunit, p]) (stageSplit2 e)
	end
	
and stageSplit2 exp = 
	let
		val split = stageSplit2
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
		| E2call (f, e) => mapbr (fn r => Eapp (Evar f, r)) (split e)
		| E2unit => (Eunit, Tprod [], (dummy (), Eunit))
		| E2tuple e12 => splitBin e12 (fn (a,b) => Etuple [a,b])
		| E2pi (lr, e) => mapbr (fn r => Epi (case lr of Left => 0 | Right => 1, r)) (split e)
		| E2inj (lr, t, e) => mapbr (fn r => Einj (lr,Tgap,r)) (split e)
		| E2case (p,(x1,e1),(x2,e2)) => 
			let
				val (l, splitBind) = splitSubs ()
				val (pp, tp, boundP) = splitBind p  0
				val (p1, t1, bound1) = splitBind e1 1
				val (p2, t2, bound2) = splitBind e2 2
			in
				(Etuple [pp, p1, p2], Tprod [tp, t1, t2], (l, Ecase (boundP, (x1,bound1), (x2,bound2))))
			end
		| E2prev e => mapp (fn c => Epi (1, c)) (stageSplit1 e)
	end
	
end
