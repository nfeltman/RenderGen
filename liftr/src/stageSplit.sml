
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
  
(* map precomp / residual *)
fun mapSnd f (a,b) = (a, f b)
fun index Left = 0
  | index Right = 1

val Eunit = Etuple []
fun bind e b = Elet (e,b)

fun freshPi () = 
	let
		val l = Variable.newvar "l"
	in
		(l, fn i => Epi (i, Evar l))
	end

fun chain (e1,e2) = Epi (1, Etuple [e1,e2])

datatype splitResult1 = NoPrec1 of expr * expr | WithPrec1 of expr * ty * (var * expr)
datatype splitResult2 = NoPrec2 of expr | WithPrec2 of expr * ty * (var * expr)
fun coerce1 (WithPrec1 r) = r
  | coerce1 (NoPrec1 (e1,e2)) = (Etuple [e1, Eunit], Tprod [], (Variable.newvar "dummy", e2))

(* assume here that the expression already type-checks *)
fun stageSplit1 (E1 exp) = 
	let
		val (split, coerce) = (stageSplit1, coerce1)
		fun bindProj e f = 
			let
				val (v,pi) = freshPi ()
			in
				bind e (v, f (pi 0, pi 1))
			end
	in
		case exp of 
		  Fvar v => NoPrec1 (Evar v, Evar v)
		| Funit => NoPrec1 (Eunit, Eunit)
		| Fint i => NoPrec1 (Eint i, Eunit)
		| Fbool b => NoPrec1 (Ebool b, Eunit)
		| Ftuple (e1, e2) => 
			let
				val (link, pi) = freshPi ()
				val (c1, tb1, lr1) = coerce (split e1)
				val (c2, tb2, lr2) = coerce (split e2)
			in
				WithPrec1 (
					bindProj c1 (fn (val1,pre1) =>
						bindProj c2 (fn (val2,pre2) =>
							Etuple [Etuple[val1,val2], Etuple[pre1,pre2]]
					)), 
					Tprod[tb1,tb2], 
					(link, Etuple [bind (pi 0) lr1, bind (pi 1) lr2])
				)
			end
		| Fpi (side, e) => 
			let
				val (c, tb, lr) = coerce (split e)
				fun proj x = Epi (index side, x)
			in
				WithPrec1 (
					bindProj c (fn (e1,e2) => Etuple[proj e1, e2]), 
					tb,
					mapSnd proj lr
				)
			end
		| Fif (e1, e2, e3) => 
			let
				val (link, pi) = freshPi ()
				val (c1, tb1, lr1) = coerce (split e1)
				val (c2, tb2, lr2) = coerce (split e2)
				val (c3, tb3, lr3) = coerce (split e3)
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
				val (c1, tb1, lr1) = coerce (split e1)
				val (c2, tb2, lr2) = coerce (split e2)
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
				Etuple[Ebinop(bo, Epi(0, #1 (coerce (split e1))), Epi(0, #1 (coerce (split e2)))), Etuple[]],
				Tprod[], 
				(Variable.newvar "dummy", Etuple[])
			)
		| Ferror t => 
			let
				val first = firstImage t
				val second = secondImage t
			in
				NoPrec1 (Eerror (first), Eerror second)
			end
	end
  | stageSplit1 (E1next e) =(
		case stageSplit2 e of
		  NoPrec2 r => NoPrec1 (Eunit, r)
		| WithPrec2 (p,t,b) => WithPrec1 (Etuple [Eunit, p],t,b))
  | stageSplit1 (E1hold e) =
		let
			val (link, pi) = freshPi ()
			val (c, tb, lr) = coerce1 (stageSplit1 e)
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
		fun merge1 (NoPrec2 r) f = NoPrec2 (f r)
		  | merge1 (WithPrec2 (c,b,(l,r))) f = WithPrec2 (c,b,(l,f r))
		fun merge2 (NoPrec2 e1, res) f = merge1 res (fn x => f (e1, x))
		  | merge2 (res, NoPrec2 e2) f = merge1 res (fn x => f (x, e2))
		  | merge2 (WithPrec2 (p1,t1,b1), WithPrec2 (p2,t2,b2)) f = 
				let
					val (l, pi) = freshPi ()
				in
					WithPrec2 (Etuple [p1,p2], Tprod [t1,t2], (l, f (bind (pi 0) b1, bind (pi 1) b2)))
				end
		fun merge3 (NoPrec2 e1, res2, res3) f = merge2 (res2, res3) (fn (x,y) => f (e1, x, y))
		  | merge3 (res1, NoPrec2 e2, res3) f = merge2 (res1, res3) (fn (x,y) => f (x, e2, y))
		  | merge3 (res1, res2, NoPrec2 e3) f = merge2 (res1, res2) (fn (x,y) => f (x, y, e3))
		  | merge3 (WithPrec2 (p1,t1,b1), WithPrec2 (p2,t2,b2), WithPrec2 (p3,t3,b3)) f = 
				let
					val (l, pi) = freshPi ()
				in
					WithPrec2 (Etuple [p1,p2,p3], Tprod [t1,t2,t3], 
						(l, f (bind (pi 0) b1, bind (pi 1) b2, bind (pi 2) b3))
					)
				end
	in
		case exp of 
		  Fvar v => NoPrec2 (Evar v)
		| Funit => NoPrec2 Eunit
		| Fint i => NoPrec2 (Eint i)
		| Fbool b => NoPrec2 (Ebool b)
		| Ftuple (e1, e2) => merge2 (split e1, split e2) (fn (a,b) => Etuple [a,b])
		| Fpi (lr, e) => merge1 (split e) (fn r => Epi (index lr, r))
		| Fif (e1, e2, e3) => merge3 (split e1, split e2, split e3) (fn (a,b,c) => Eif (a,b,c))
		| Fbinop (bo,e1,e2) => merge2 (split e1, split e2) (fn (a,b) => Ebinop(bo,a,b))
		| Flet (e1,(x, e2)) => merge2 (split e1, split e2) (fn (a,b) => Elet(a,(x,b)))
		| Ferror t => NoPrec2 (Eerror (trType2 t))
	end
  | stageSplit2 (E2prev e) =
			let
				val (c, tb, lr) = coerce1 (stageSplit1 e)
			in
				WithPrec2 (Epi (1, c),tb,lr)
			end
end
