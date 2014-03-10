
structure StageSplit = 
struct

open LangCommon
open LambdaPSF
open Typecheck12

fun trType2 (T2 TFint) = Tint
  | trType2 (T2 TFbool) = Tbool
  | trType2 (T2 TFunit) = Tprod []
  | trType2 (T2 (TFprod (t1,t2))) = Tprod [trType2 t1, trType2 t2]

fun firstImage (T1 TFint) = Tint
  | firstImage (T1 TFbool) = Tbool
  | firstImage (T1 TFunit) = Tprod []
  | firstImage (T1 (TFprod (t1,t2))) = Tprod [firstImage t1, firstImage t2]
  | firstImage (T1fut _) = Tprod []

fun secondImage (T1 TFint) = Tprod []
  | secondImage (T1 TFbool) = Tprod []
  | secondImage (T1 TFunit) = Tprod []
  | secondImage (T1 (TFprod (t1,t2))) = Tprod [firstImage t1, firstImage t2]
  | secondImage (T1fut t) = trType2 t
  
  
fun fresh () = Variable.newvar "l"
fun dummy () = Variable.newvar "dummy"

exception BadType
datatype entry = Func1 of type1 * ty * type1 | Val1 of type1 | Val2 of type2
fun unval1 (Val1 t) = t
  | unval1 _ = raise BadType
fun unval2 (Val2 t) = t
  | unval2 _ = raise BadType

(* map precomp / residual *)
fun mapSnd f (a,b) = (a, f b)
fun mapp f (a,b,c,d) = (f a, b, c, d)

val Eunit = Etuple []
fun bind e b = Elet (e,b)
fun deadBranch t = (dummy (), Eerror t)

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

fun chain (e1,e2) = Epi (1, Etuple [e1,e2])

datatype splitResult = NoPrec1 of expr * expr | WithPrec1 of expr * ty * (var * expr)

(* assume here that the expression already type-checks *)
fun stageSplit1 (E1 exp) = 
	let
		fun split x = case stageSplit1 x of WithPrec1 r => r
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
		  Fvar v => WithPrec1 (Etuple [Evar v, Eunit], Tprod [], (dummy (), Evar v))
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
		| Funit => WithPrec1 (Etuple [Eunit, Eunit], Tprod [], (dummy (), Eunit))
		| Fint i => WithPrec1 (Etuple [Eint i, Eunit], Tprod [], (dummy (), Eunit))
		| Fbool b => WithPrec1 (Etuple [Ebool b, Eunit], Tprod [], (dummy (), Eunit))
		| Ftuple (e1, e2) => 
			let
				val (link, splitBind) = splitSubs ()
				val (c1, tb1, bound1) = splitBind e1 0
				val (c2, tb2, bound2) = splitBind e2 1
			in
				WithPrec1 (
					bindProj c1 (fn (val1,pre1) =>
						bindProj c2 (fn (val2,pre2) =>
							Etuple [Etuple[val1,val2], Etuple[pre1,pre2]]
					)), 
					Tprod[tb1,tb2], 
					(link, Etuple [bound1, bound2])
				)
			end
		| Fpi (side, e) => 
			let
				val (c, tb, lr) = split e
				fun proj x = Epi (case side of Left => 0 | Right => 1, x)
			in
				WithPrec1 (
					bindMap c proj id,
					tb,
					mapSnd proj lr
				)
			end
	(*	| Finj (side, otherT, e) => 
			let
				val (c, tb, lr) = split e
				fun inj t x = Einj (side, t, x)
			in
				(
					bindMap c (inj (firstImage otherT)) id,
					tb,
					mapSnd (inj (secondImage otherT)) lr
				)
			end
		| Fcase (e,(x1,e1),(x2,e2)) =>
			let
				val (link, pi) = freshPi ()
				val (c,  tb,  lr,  t ) = split e
				val (tLeft, tRight) = Typecheck12.unsum1 t
				val (c1, tb1, lr1, t1) = stageSplit1 (extendContext gamma x1 (Val1 tLeft))  e1
				val (c2, tb2, lr2, _ ) = stageSplit1 (extendContext gamma x2 (Val1 tRight)) e2
				val outType = secondImage t1
				fun inj side t x = Einj (side, t, x)
			in
				(
					bindProj c (fn (predVal,predPre) =>
						bindProj
						(Ecase(predVal, 
							(x1, bindMap c1 id (inj Left  tb2)), 
							(x2, bindMap c2 id (inj Right tb1))
						))
						(fn (y1,y2) => Etuple [y1, Etuple [predPre,y2]])
					), 
					Tprod [tb, Tsum (tb1,tb2)],
					(link,  
						Ecase (bind (pi 0) lr,
							(x1, Ecase (pi 1, lr1, deadBranch outType)),
							(x2, Ecase (pi 1, deadBranch outType, lr2))
						)
					),
					t1
				)
			end *)
		| Fif (e1, e2, e3) => 
			let
				val (link, pi) = freshPi ()
				val (c1, tb1, lr1) = split e1
				val (c2, tb2, lr2) = split e2
				val (c3, tb3, lr3) = split e3
			in
				WithPrec1 (
					bindProj c1 (fn (val1,pre1) =>
						Eif(val1, 
							bindProj c2 (fn (val2,pre2) => Etuple[val2, Etuple[pre1,Einj(Left, tb3,pre2)]]),
							bindProj c3 (fn (val3,pre3) => Etuple[val3, Etuple[pre1,Einj(Right,tb2,pre3)]])
						)
					), 
					Tprod[tb1,Tsum(tb2,tb3)], 
					(link, chain (bind (pi 0) lr1, Ecase(pi 1,lr2,lr3)))
				)
			end
		| Flet (e1, (x,e2)) => 
			let
				val (link, pi) = freshPi ()
				val (c1, tb1, lr1) = split e1
				val (c2, tb2, lr2) = split e2
			in
				WithPrec1 (
					bindProj c1 (fn (val1,pre1) =>
						bindProj (Elet (val1,(x,c2))) (fn (val2,pre2) =>
							Etuple [val2, Etuple[pre1,pre2]]
					)), 
					Tprod[tb1,tb2], 
					(link, bind (bind (pi 0) lr1) (x,bind (pi 1) lr2))
				)
			end
		| Fbinop (bo,e1,e2) =>
			WithPrec1 (
				Etuple[Ebinop(bo, Epi(0, #1 (split e1)), Epi(0, #1 (split e2))), Etuple[]],
				Tprod[], 
				(dummy (), Etuple[])
			)
		| Ferror t => 
			let
				val first = firstImage t
				val second = secondImage t
			in
				WithPrec1 (Eerror (Tprod [first, Tprod[]]), Tprod[], (dummy (), Eerror second))
			end
	end
  | stageSplit1 (E1next e) =
		let
			val (p, tb, lr) = stageSplit2 e
		in
			WithPrec1 (Etuple [Eunit, p],tb,lr)
		end
  | stageSplit1 (E1hold e) =
		let
			val (link, pi) = freshPi ()
			val WithPrec1 (c, tb, lr) = stageSplit1 e
		in
			WithPrec1 (
				Etuple [Eunit,c], 
				Tprod [Tint,tb], 
				(link, chain (bind (pi 1) lr, pi 0))
			)
		end
	
and stageSplit2 (E2 exp) = 
	let
		val split = stageSplit2
		fun splitBr (v,e) = (v, split e)
		val splitSubs = splitSubsBase split
		fun splitBin (e1, e2) operation = 
			let
				val (l,splitBind) = splitSubs ()
				val (p1,tb1,bound1) = splitBind e1 0
				val (p2,tb2,bound2) = splitBind e2 1
				val r = operation (bound1, bound2)
			in
				(Etuple [p1,p2], Tprod [tb1,tb2], (l,r))
			end
		fun mapbr f (c,b,(l,r)) = (c,b,(l,f r))
	in
		case exp of 
		  Fvar v => (Eunit, Tprod [], (dummy (), Evar v))
	(*	| Flam (t,(x,e)) => mapbr (fn r => Elam (Tgap,(x,r))) (split e)
		| Fapp e12 => splitBin e12 Eapp *)
		| Funit => (Eunit, Tprod [], (dummy (), Eunit))
		| Fint i => (Eunit, Tprod [], (dummy (), Eint i))
		| Fbool b => (Eunit, Tprod [], (dummy (), Ebool b))
		| Ftuple e12 => splitBin e12 (fn (a,b) => Etuple [a,b])
		| Fpi (lr, e) => mapbr (fn r => Epi (case lr of Left => 0 | Right => 1, r)) (split e)
	(*	| Finj (lr, ot, e) => mapbr (fn (r,t) => Einj (lr, trType2 ot, r)) (split e)
		| Fcase (p,(x1,e1),(x2,e2)) => 
			let
				val (link, pi) = freshPi ()
				val (pp, tbp, lrp) = split p
				val (p1, tb1, lr1) = split e1
				val (p2, tb2, lr2)  = split e2
			in
				(Etuple [pp, p1, p2], Tprod [tbp, tb1, tb2], (link, 
					Ecase (bind (pi 0) lrp, 
						(x1,bind (pi 1) lr1), 
						(x2,bind (pi 2) lr2))
				))
			end *)
		| Fif (e1, e2, e3) => 
			let
				val (link, pi) = freshPi ()
				val (pp, tbp, lrp) = split e1
				val (p1, tb1, lr1) = split e2
				val (p2, tb2, lr2)  = split e3
			in
				(Etuple [pp, p1, p2], Tprod [tbp, tb1, tb2], (link, 
					Eif (bind (pi 0) lrp, 
						 bind (pi 1) lr1, 
						 bind (pi 2) lr2)
				))
			end
		| Fbinop (bo,e1,e2) => splitBin (e1,e2) (fn (a,b) => Ebinop(bo,a,b))
		| Flet (e1, (x, e2)) => 
			let
				val (link, pi) = freshPi ()
				val (p1, tb1, lr1) = split e1
				val (p2, tb2, lr2) = stageSplit2 e2
			in
				(Etuple [p1, p2], Tprod [tb1, tb2], (link, 
					Elet (bind (pi 0) lr1, (x,bind (pi 1) lr2))
				))
			end
		| Ferror t => (Eerror (Tprod []), Tprod[], (dummy (), Eerror (trType2 t)))
	end
  | stageSplit2 (E2prev e) =
			let
				val WithPrec1 (c, tb, lr) = stageSplit1 e
			in
				(Epi (1, c),tb,lr)
			end
end
