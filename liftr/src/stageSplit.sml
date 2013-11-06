
structure StageSplit = 
struct

open LangCommon
open LambdaPSF
open Typecheck12

fun fresh () = "l"
fun dummy () = "dummy"

(* map precomp / residual *)
fun mapp f (c,b) = (f c,b)
fun mapSnd f (a,b) = (a, f b)
fun mapbr f (c,(l,r)) = (c,(l,f r))

val Eunit = Etuple []
fun bind _ _ = Eunit

fun freshPi split () = 
	let
		val l = fresh ()
	in
		(l, fn i => Epi (i, Evar l))
	end

fun splitSubsBase split () = 
	let
		val (l,pi) = freshPi ()
		fun splitBind e i = mapSnd (bind (pi i)) (split e)
	in
		(l, splitBind)
	end

(* assume here that the expression already type-checks *)
fun stageSplit1 exp = 
	let
		val split = stageSplit1
		val splitBindBase splitBind e i = 
			let
				val v = fresh ()
				val (c,bound) = splitBind e i
			in
				(Epi (0, Evar v), Epi (1, Evar v), fn rest => bind c (v, rest), bound)
			end
		fun splitSubs () = mapSnd (fn => splitBindBase splitBind) (splitSubsBase split ())
	in
		case exp of 
		  E2var v => (Etuple [Evar v, Eunit], (dummy (), Evar v))
		| E2lam (t,(x,e)) => 
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
			end
		| E2unit => (Etuple [Eunit, Eunit], (dummy (), Eunit))
		| E2tuple (e1, e2) => 
			let
				val (l,splitDecompBind) = splitSubs ()
				val (c11,c12,letc1,bound1) = splitDecompBind e1 0
				val (c21,c22,letc2,bound2) = splitDecompBind e2 1
			in
				(
					letc1 (letc2 (Etuple [Etuple[c11,c21], Etuple[c12,c22]])), 
					(l, Etuple [bound1, bound2])
				)
			end
		| E2pi (side, e) => 
			let
				val (v, pi) = freshPi ()
				val (c, lr) = split e
				fun proj x = Epi (case side of Left => 0 | Right => 1, x)
			in
				(
					bind c (v, Etuple[proj (pi 0), pi 1]), 
					mapSnd proj lr
				)
			end
		| E2inj (side, _, e) => 
			let
				val (v, pi) = freshPi ()
				val (c, lr) = split e
				fun inj t x = Einj (side, t, x)
			in
				(
					bind c (v, Etuple[inj Tgap (pi 0), pi 1]), 
					mapSnd (inj Tgap) lr
				)
			end
		| E2case (p,(x1,e1),(x2,e2)) => 
			let
				val (l, splitDecompBind) = splitSubs ()
				val (pp, boundP) = splitDecompBind p  0
				val (c11, c12, letc1, bound1) = splitDecompBind e1 1
				val (c21, c22, letc2, bound2) = splitDecompBind e2 2
			in
				(Etuple [pp, p1, p2], (l, Ecase (boundP, (x1,bound1), (x2,bound2))))
			end
		| E2prev e => mapp (fn c => Epi (1, c)) (stageSplit1 e)
	end
	
and stageSplit2 exp = 
	let
		val split = stageSplit2
		fun splitBr (v,e) = (v, split e)
		val splitSubs = splitSubsBase split
		fun splitBin (e1, e2) operation = 
			let
				val (l,splitBind) = splitSubs ()
				val (p1,bound1) = splitBind e1 0
				val (p2,bound2) = splitBind e2 1
			in
				(Etuple [p1,p2], (l, operation (bound1, bound2) ))
			end
	in
		case exp of 
		  E2var v => (Eunit, (dummy (), Evar v))
		| E2lam (t,(x,e)) => mapbr (fn r => Elam (Tgap,(x,r))) (split e)
		| E2app e12 => splitBin e12 Eapp
		| E2unit => (Eunit, (dummy (), Eunit))
		| E2tuple e12 => splitBin e12 (fn (a,b) => Etuple [a,b])
		| E2pi (lr, e) => mapbr (fn r => Epi (case lr of Left => 0 | Right => 1, r)) (split e)
		| E2inj (lr, t, e) => mapbr (fn r => Einj (lr,Tgap,r)) (split e)
		| E2case (p,(x1,e1),(x2,e2)) => 
			let
				val (l, splitBind) = splitSubs ()
				val (pp, boundP) = splitBind p  0
				val (p1, bound1) = splitBind e1 1
				val (p2, bound2) = splitBind e2 2
			in
				(Etuple [pp, p1, p2], (l, Ecase (boundP, (x1,bound1), (x2,bound2))))
			end
		| E2prev e => mapp (fn c => Epi (1, c)) (stageSplit1 e)
	end
	
end
