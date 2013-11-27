
structure StageSplit = 
struct

open LangCommon
open LambdaPSF
open Typecheck12

fun trType2 T2int = Tint
  | trType2 T2bool = Tbool
  | trType2 T2unit = Tprod []
  | trType2 (T2prod (t1,t2)) = Tprod [trType2 t1, trType2 t2]
  | trType2 (T2sum (t1,t2)) = Tsum (trType2 t1, trType2 t2)

fun firstImage T1int = Tint
  | firstImage T1bool = Tbool
  | firstImage T1unit = Tprod []
  | firstImage (T1prod (t1,t2)) = Tprod [firstImage t1, firstImage t2]
  | firstImage (T1sum (t1,t2)) = Tsum (firstImage t1, firstImage t2)
  | firstImage (T1fut _) = Tprod []

fun secondImage T1int = Tprod []
  | secondImage T1bool = Tprod []
  | secondImage T1unit = Tprod []
  | secondImage (T1prod (t1,t2)) = Tprod [firstImage t1, firstImage t2]
  | secondImage (T1sum (t1,t2)) = Tsum (firstImage t1, firstImage t2)
  | secondImage (T1fut t) = trType2 t
  
  
fun fresh () = Variable.newvar "l"
fun dummy () = Variable.newvar "dummy"

datatype entry = Func1 of type1 * ty * type1 | Val1 of type1 | Val2 of type2
fun unval1 (Val1 t) = t
fun unval2 (Val2 t) = t

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
		fun mapThird f (a,b,c, d) = (a, b, f c, d)
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
		  E1var v => (Etuple [Evar v, Eunit], Tprod [], (dummy (), Evar v), unval1 (lookup gamma v))
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
				val (c,tb,bound,_) = splitBind e 0
				val Func1 (_,boundary,t2) = lookup gamma f
			in
				(
					bindProj c (fn (argVal,argPre) =>
						bindProj (Eapp (Evar f, argVal)) (fn (resVal,resBoun) => 
							Etuple [resVal, Etuple [argPre,resBoun]]
					)), 
					Tprod[tb, boundary],
					(link, Eapp (Evar f, Etuple [bound, Epi(1, Evar link)])),
					t2
				)
			end
		| E1unit => (Etuple [Eunit, Eunit], Tprod [], (dummy (), Eunit), T1unit)
		| E1tuple (e1, e2) => 
			let
				val (link, splitBind) = splitSubs ()
				val (c1, tb1, bound1, t1) = splitBind e1 0
				val (c2, tb2, bound2, t2) = splitBind e2 1
			in
				(
					bindProj c1 (fn (val1,pre1) =>
						bindProj c2 (fn (val2,pre2) =>
							Etuple [Etuple[val1,val2], Etuple[pre1,pre2]]
					)), 
					Tprod[tb1,tb2], 
					(link, Etuple [bound1, bound2]),
					T1prod(t1,t2)
				)
			end
		| E1pi (side, e) => 
			let
				val (c, tb, lr, t) = split e
				fun proj x = Epi (case side of Left => 0 | Right => 1, x)
			in
				(
					bindMap c proj id,
					tb,
					mapSnd proj lr,
					projLR side (Typecheck12.unprod1 t)
				)
			end
		| E1inj (side, otherT, e) => 
			let
				val (c, tb, lr, t) = split e
				fun inj t x = Einj (side, t, x)
			in
				(
					bindMap c (inj (firstImage otherT)) id,
					tb,
					mapSnd (inj (secondImage otherT)) lr,
					T1sum (injLR side t otherT)
				)
			end
		| E1case (e,(x1,e1),(x2,e2)) =>
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
			end
		| E1binop (bo,e1,e2) =>
			(
				Etuple[Ebinop(bo, Epi(0, #1 (split e1)), Epi(0, #1 (split e2))), Etuple[]],
				Tprod[], 
				(dummy (), Etuple[]),
				#3 (Prim1.getTypes bo)
			)
		| E1next e => 
			let
				val (p, tb, lr, t) = stageSplit2 gamma e
			in
				(Etuple [Eunit, p],tb,lr, T1fut t)
			end
	end
	
and stageSplit2 gamma exp = 
	let
		val split = stageSplit2 gamma
		fun splitBr (v,e) = (v, split e)
		val splitSubs = splitSubsBase split
		fun splitBin (e1, e2) operation = 
			let
				val (l,splitBind) = splitSubs ()
				val (p1,tb1,bound1,t1) = splitBind e1 0
				val (p2,tb2,bound2,t2) = splitBind e2 1
				val (r,t) = operation (bound1, t1, bound2, t2)
			in
				(Etuple [p1,p2], Tprod [tb1,tb2], (l,r), t)
			end
		fun mapbr f (c,b,(l,r),t) = case f (r,t) of (r2,t2) => (c,b,(l,r2),t2)
	in
		case exp of 
		  E2var v => (Eunit, Tprod [], (dummy (), Evar v), unval2 (lookup gamma v))
	(*	| E2lam (t,(x,e)) => mapbr (fn r => Elam (Tgap,(x,r))) (split e)
		| E2app e12 => splitBin e12 Eapp *)
	(*	| E2call (f, e) => mapbr (fn r => Eapp (Evar f, r)) (split e) *)
		| E2unit => (Eunit, Tprod [], (dummy (), Eunit), T2unit)
		| E2tuple e12 => splitBin e12 (fn (a,ta,b,tb) => (Etuple [a,b], T2prod(ta,tb)))
		| E2pi (lr, e) => mapbr (fn (r,t) => (Epi (case lr of Left => 0 | Right => 1, r), projLR lr (Typecheck12.unprod2 t))) (split e)
		| E2inj (lr, ot, e) => mapbr (fn (r,t) => (Einj (lr, trType2 ot, r), T2sum (injLR lr t ot))) (split e)
		| E2case (p,(x1,e1),(x2,e2)) => 
			let
				val (link, pi) = freshPi ()
				val (pp, tbp, lrp, tp) = split e1
				val (tLeft, tRight) = Typecheck12.unsum2 tp
				val (p1, tb1, lr1, t1) = stageSplit2 (extendContext gamma x1 (Val2 tLeft))  e1
				val (p2, tb2, lr2, _)  = stageSplit2 (extendContext gamma x2 (Val2 tRight)) e2
			in
				(Etuple [pp, p1, p2], Tprod [tbp, tb1, tb2], (link, 
					Ecase (bind (pi 0) lrp, 
						(x1,bind (pi 1) lr1), 
						(x2,bind (pi 2) lr2))
				),t1)
			end
		| E2binop (bo,e1,e2) => splitBin (e1,e2) (fn (a,_,b,_) => (Ebinop(bo,a,b), #3 (Prim2.getTypes bo)))
		| E2prev e => 
			let
				val (c, tb, lr, t) = stageSplit1 gamma e
			in
				(Epi (1, c),tb,lr, Typecheck12.unfut t)
			end
	end

fun splitProg prog = 
	let
		fun splitFunc _ [] = (Eunit, Eunit)
		  | splitFunc g (FuncDec1(f,t1,t2,v,e) :: rest) = 
			let
				val (c,boundary,(l,r),_) = stageSplit1 (extendContext g v (Val1 t1)) e
				val (rest1, rest2) = splitFunc (extendContext g f (Func1 (t1,boundary,t2))) rest
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
