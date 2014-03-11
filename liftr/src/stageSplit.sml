
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
fun freshPi2 () = 
	let
		val l = Variable.newvar "l"
	in
		(l, Epi (0, Evar l), Epi (1, Evar l))
	end

fun chain (e1,e2) = Epi (1, Etuple [e1,e2])

datatype splitResult1 = NoPrec1 of expr * expr | WithPrec1 of expr * ty * (var * expr)
datatype splitResult2 = NoPrec2 of expr | WithPrec2 of expr * ty * (var * expr)
fun coerce1 (WithPrec1 r) = r
  | coerce1 (NoPrec1 (e1,e2)) = (Etuple [e1, Eunit], Tprod [], (Variable.newvar "dummy", e2))

(* assume here that the expression already type-checks *)
fun stageSplit1 (E1 exp) = 
	let
		val split = stageSplit1
		fun bindProj e f = 
			let
				val (v,pi) = freshPi ()
			in
				bind e (v, f (pi 0, pi 1))
			end
		fun id x = x
		fun app x f = f x
		fun merge1 (NoPrec1 (e,r)) f g = NoPrec1 (f (app e, id), g r)
		  | merge1 (WithPrec1 (c,t,(l,r))) f g = 
				let
					val (y, e, p) = freshPi2 ()
				in
					WithPrec1 (f (fn x => Elet(c, (y, x e)), fn x => Etuple[x, p]), t, (l, g r))
				end
		fun merge2 (NoPrec1 (e1, r1), res) f g = merge1 res (fn (bind2,wrap) => f (app e1,bind2,wrap)) (fn r2 => g (r1, r2))
		  | merge2 (res, NoPrec1 (e2, r2)) f g = merge1 res (fn (bind1,wrap) => f (bind1,app e2,wrap)) (fn r1 => g (r1, r2))
		  | merge2 (WithPrec1 (c1,t1,b1), WithPrec1 (c2,t2,b2)) f g = 
				let
					val (link, l1, l2) = freshPi2 ()
					val (y1, e1, p1) = freshPi2 ()
					val (y2, e2, p2) = freshPi2 ()
				in
					WithPrec1 (
					f (
						fn x => Elet(c1, (y1, x e1)), 
						fn x => Elet(c2, (y2, x e2)), 
						fn x => Etuple[x, Etuple[p1,p2]]
					), Tprod[t1,t2], 
					(link, g (bind l1 b1, bind l2 b2))
				)
				end
		fun unpackPredicate (NoPrec1 (e,r)) = (app e, id, id, fn _ => r, Evar)
		  | unpackPredicate (WithPrec1 (c,t,lr)) = 
				let
					val (y,e,p) = freshPi2 ()
				in
					(fn x => Elet(c,(y,x e)), 
					fn x => Etuple[p,x], 
					fn x => Tprod[t,x], 
					fn v => Elet(Epi(0,Evar v),lr), 
					fn v => Epi(1,Evar v))
				end
		  
		fun makeTup (a,b) = Etuple [a,b]
		fun simpleMerge2 e12 f g = merge2 e12 (fn (bind1, bind2, wrap) => bind1 (fn e1 => bind2 (fn e2 => wrap(f (e1,e2))))) g
	in
		case exp of 
		  Fvar v => NoPrec1 (Evar v, Evar v)
		| Funit => NoPrec1 (Eunit, Eunit)
		| Fint i => NoPrec1 (Eint i, Eunit)
		| Fbool b => NoPrec1 (Ebool b, Eunit)
		| Ftuple (e1, e2) => simpleMerge2 (split e1, split e2) makeTup makeTup
		| Fpi (side, e) => 
			let
				fun proj x = Epi (index side, x)
			in
				merge1 (split e) (fn (bind,wrap) => bind (wrap o proj)) proj
			end
		| Fif (e1, e2, e3) => 
			let
				val link = Variable.newvar "l"
				val (c2, tb2, lr2) = coerce1 (split e2)
				val (c3, tb3, lr3) = coerce1 (split e3)
				val (bind1, addPrec, addTy, predResi, resiPred) = unpackPredicate (split e1)
			in
				WithPrec1 (
					bind1 (fn val1 =>
						Eif(val1, 
							bindProj c2 (fn (val2,pre2) => Etuple[val2, addPrec(Einj(Left, tb3,pre2))]),
							bindProj c3 (fn (val3,pre3) => Etuple[val3, addPrec(Einj(Right,tb2,pre3))])
						)
					), 
					addTy(Tsum(tb2,tb3)), 
					(link, chain (predResi link, Ecase(resiPred link,lr2,lr3)))
				)
			end
		| Flet (e1, (x,e2)) => 
			merge2 (split e1, split e2) 
				(fn (bind1, bind2, wrap) => bind1 (fn e1 => Elet (e1, (x, bind2 wrap))))
				(fn (r1,r2) => Elet(r1,(x,r2)))
		| Fbinop (bo,e1,e2) =>
			simpleMerge2 (split e1, split e2) (fn (a,b) => Ebinop(bo,a,b)) (fn (r1,r2) => Epi(2,Etuple[r1,r2,Etuple[]]))
		| Ferror t => NoPrec1 (Eerror (firstImage t), Eerror (secondImage t))
	end
  | stageSplit1 (E1next e) =(
		case stageSplit2 e of
		  NoPrec2 r => NoPrec1 (Eunit, r)
		| WithPrec2 (p,t,b) => WithPrec1 (Etuple [Eunit, p],t,b))
  | stageSplit1 (E1hold e) =
		let
			val (link, pi) = freshPi ()
		in
			case stageSplit1 e of
			  NoPrec1 (i, r) => WithPrec1 (Etuple [Eunit,i], Tint, (link, chain (r, Evar link)))
			| WithPrec1 (c, tb, lr) =>
				WithPrec1 (Etuple [Eunit,c], Tprod [Tint,tb], (link, chain (bind (pi 1) lr, pi 0)))
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
  | stageSplit2 (E2prev e) = (
		case stageSplit1 e of
		  NoPrec1 (Etuple [], r) => NoPrec2 r
		| NoPrec1 (Evar _, r) => NoPrec2 r
		| NoPrec1 (c,r) => WithPrec2 (chain (c,Eunit), Tprod [], (Variable.newvar "dummy", r))
		| WithPrec1 (c, tb, lr) => WithPrec2 (Epi (1, c),tb,lr))
end
