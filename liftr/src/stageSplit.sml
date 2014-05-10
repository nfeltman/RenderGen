
structure StageSplit = 
struct

open LangCommon
open LambdaPSF
open Typecheck12

(*
fun trType2 (T2 TFint) = Tint
  | trType2 (T2 TFbool) = Tbool
  | trType2 (T2 TFunit) = Tprod []
  | trType2 (T2 (TFsum  (t1,t2))) = Tsum  (trType2 t1, trType2 t2)
  | trType2 (T2 (TFprod (t1,t2))) = Tprod [trType2 t1, trType2 t2]

  
fun firstImage (T1 TFint) = Tint
  | firstImage (T1 TFbool) = Tbool
  | firstImage (T1 TFunit) = Tprod []
  | firstImage (T1 (TFsum  (t1,t2))) = Tsum  (firstImage t1, firstImage t2)
  | firstImage (T1 (TFprod (t1,t2))) = Tprod [firstImage t1, firstImage t2]
  | firstImage (T1fut _) = Tprod []

  
fun secondImage (T1 TFint) = Tprod []
  | secondImage (T1 TFbool) = Tprod []
  | secondImage (T1 TFunit) = Tprod []
  | secondImage (T1 (TFsum  (t1,t2))) = (* if type here *)
  | secondImage (T1 (TFprod (t1,t2))) = Tprod [firstImage t1, firstImage t2]
  | secondImage (T1fut t) = trType2 t *)
  
fun trType2 _ = ()
fun firstImage _ = ()
fun secondImage _ = ()

fun convertPattern (Pvar v) = PPvar v
  | convertPattern (Ptuple (p1,p2)) = PPtuple [convertPattern p1, convertPattern p2]
  
fun index Left = 0
  | index Right = 1

val Eunit = Etuple []
fun bind e1 (x,e2) = Elet (e1,(x, e2))

fun freshPi () = 
	let
		val l = Variable.newvar "l"
	in
		(l, fn i => Epi (i, Evar l))
	end
fun freshPat2 n1 n2 = 
	let
		val (v,p) = (Variable.newvar n1, Variable.newvar n2)
	in
		(PPtuple[PPvar v, PPvar p], Evar v, Evar p)
	end
fun freshPi2 () = 
	let
		val l = Variable.newvar "l"
	in
		(l, Epi (0, Evar l), Epi (1, Evar l))
	end

fun terminates e = (case e of
	Evar _ => true
  | Eint _ => true
  | Ebool _ => true
  | Elam _ => true
  | Eapp _ => false
  | Etuple es => List.all terminates es
  | Epi (_,e) => terminates e
  | Einj (_,_,e) => terminates e
  | Ecase (e1,(_,e2),(_,e3)) => (terminates e1) andalso (terminates e2) andalso (terminates e3)
  | Eif (e1,e2,e3) => (terminates e1) andalso (terminates e2) andalso (terminates e3)
  | Elet (e1,(_,e2)) => (terminates e1) andalso (terminates e2)
  | Ebinop (_,e1,e2) => (terminates e1) andalso (terminates e2)
  | Eroll e => terminates e
  | Eunroll e => terminates e
  | Eerror _ => false)

fun chain2 (e1,e2) = if terminates e1 then e2 else Epi (1, Etuple [e1,e2])
fun chain3 (e1,e2,e3) = 
	case (terminates e1, terminates e2) of
	  (true,  true ) => e3
	| (true,  false) => Epi (1, Etuple [e2,e3])
	| (false, true ) => Epi (1, Etuple [e1,e3])
	| (false, false) => Epi (2, Etuple [e1,e2,e3])

datatype 't splitResult1 = NoPrec1 of 't expr * 't expr | WithPrec1 of 't expr * ty * (ppatt * 't expr)
datatype 't splitResult2 = NoPrec2 of 't expr | WithPrec2 of 't expr * ty * (ppatt * 't expr)
fun coerce1 (WithPrec1 res) = res
  | coerce1 (NoPrec1 (e1,e2)) = (Etuple [e1, Eunit], Tprod [], (PPtuple [], e2))

(* assume here that the expression already type-checks *)
fun stageSplit1 (E1 exp) = 
	let
		val split = stageSplit1 : expr1 -> unit splitResult1
		fun id x = x
		fun app x f = f x
		fun merge1 (NoPrec1 (e,r)) f g = NoPrec1 (f (app e, id), g r)
		  | merge1 (WithPrec1 (c,t,(l,r))) f g = 
				let
					val (pat, v, p) = freshPat2 "v" "p"
				in
					WithPrec1 (f (fn x => Elet(c, (pat, x v)), fn x => Etuple[x, p]), t, (l, g r))
				end
		fun merge2 (NoPrec1 (e1, r1), res) f g = merge1 res (fn (bind2,wrap) => f (app e1,bind2,wrap)) (fn r2 => g (r1, r2))
		  | merge2 (res, NoPrec1 (e2, r2)) f g = merge1 res (fn (bind1,wrap) => f (bind1,app e2,wrap)) (fn r1 => g (r1, r2))
		  | merge2 (WithPrec1 (c1,t1,(l1,r1)), WithPrec1 (c2,t2,(l2,r2))) f g = 
				let
					val (pat1, v1, p1) = freshPat2 "v" "p"
					val (pat2, v2, p2) = freshPat2 "v" "p"
				in
					WithPrec1 (
					f (
						fn x => Elet(c1, (pat1, x v1)), 
						fn x => Elet(c2, (pat2, x v2)), 
						fn x => Etuple[x, Etuple[p1,p2]]
					), Tprod[t1,t2], 
					(PPtuple [l1,l2], g (r1, r2))
					)
				end
		fun simpleMerge2 e12 f g = merge2 e12 (fn (bind1, bind2, wrap) => bind1 (fn e1 => bind2 (fn e2 => wrap(f (e1,e2))))) g
		fun unpackPredicate (NoPrec1 (e,r)) link = (app e, id, id, r, Evar link)
		  | unpackPredicate (WithPrec1 (c,t,(l,r))) link = 
				let
					val (v,p) = (Variable.newvar "v", Variable.newvar "p")
				in
					(fn x => Elet(c,(PPtuple[PPvar v, PPvar p],x (Evar v))), 
					fn x => Etuple[Evar p,x], 
					fn x => Tprod[t,x], 
					Elet(Epi(0,Evar link),(l, r)), 
					Epi(1,Evar link))
				end
		  
		fun makeTup (a,b) = Etuple [a,b]
		fun caseBranch c addPrec side tOther = 
				case (Variable.newvar "v", Variable.newvar "p") of 
				(v,p) => Elet (c,(PPtuple[PPvar v, PPvar p],Etuple[Evar v, addPrec(Einj(side, (),Evar p))]))
		fun caseBranches addPrec (c2, t2, b2) (c3, t3, b3) = 
				(caseBranch c2 addPrec Left t3, b2, caseBranch c3 addPrec Right t2, b3, Tsum(t2,t3))
	in
		case exp of 
		  Fvar v  => NoPrec1 (Evar v,  Evar v)
		| Funit   => NoPrec1 (Eunit,   Eunit)
		| Fint i  => NoPrec1 (Eint i,  Eunit)
		| Fbool b => NoPrec1 (Ebool b, Eunit)
		| Flam (t, (x,e)) => 
			let
				val (c,t,(l,r)) = coerce1 (split e)
			in
				NoPrec1 (Elam (firstImage t, (convertPattern x, c)), Elam ((),(PPtuple[convertPattern x,l], r)))
			end
		| Fapp (e1, e2) => 
			let
				val (link, pi) = freshPi ()					
			in
				case (split e1, split e2) of
					  (NoPrec1 (v1,r1), NoPrec1 (v2,r2)) =>
						let 
							val link = Variable.newvar "l"
						in
							WithPrec1 (Eapp (v1,v2), Tprod[], (PPvar link,Eapp (r1, Etuple [r2, Evar link])))
						end
					| (res1, res2) => 
						let
							val ((c1,_,lr1),(c2,_,lr2)) = (coerce1 res1, coerce1 res2) 
							val (v1,p1) = (Variable.newvar "v", Variable.newvar "p")
							val (v2,p2) = (Variable.newvar "v", Variable.newvar "p")
							val (v3,p3) = (Variable.newvar "v", Variable.newvar "p")
						in
							WithPrec1
							(
							Elet (Etuple[c1,c2], (PPtuple [PPtuple[PPvar v1, PPvar p1], PPtuple[PPvar v2, PPvar p2]],
								Elet (Eapp (Evar v1,Evar v2), (PPtuple [PPvar v3, PPvar p3], 
									Etuple[Evar v3,Etuple[Evar p1,Evar p2,Evar p3]])))),
							Tprod[],
							(PPvar link,Eapp(Elet (pi 0, lr1),Etuple[Elet (pi 1, lr2), pi 2]))
							)
						end
			end
		| Ftuple (e1, e2) => simpleMerge2 (split e1, split e2) makeTup makeTup
		| Fpi (side, e) => 
			let
				fun proj x = Epi (index side, x)
			in
				merge1 (split e) (fn (bind,wrap) => bind (wrap o proj)) proj
			end
		| Finj (lr, t, e) => merge1 (split e) (fn (bind,wrap) => bind (fn u => wrap (Einj(lr,firstImage t,u)))) id
		| Fcase (e1, (x2,e2), (x3,e3)) => 
			let
				val link = Variable.newvar "l"
				val z = Variable.newvar "z"
				val (bind1, addPrec, addTy, predResi, prec) = unpackPredicate (split e1) link
				val (branch2, (l2,r2), branch3, (l3,r3), t) = caseBranches addPrec (coerce1 (split e2)) (coerce1 (split e3))
				val (x2, x3) = (convertPattern x2, convertPattern x3)
			in
				WithPrec1 (
					bind1 (fn val1 => Ecase(val1, (x2,branch2), (x3,branch3))), 
					addTy t, 
					(PPvar link, Elet (predResi, (PPvar z, Ecase(prec,(l2, Elet(Evar z,(x2,r2))),(l3,Elet(Evar z,(x3,r3)))))))
				)
			end
		| Fif (e1, e2, e3) => 
			let
				val link = Variable.newvar "l"
				val (bind1, addPrec, addTy, predResi, prec) = unpackPredicate (split e1) link
				val (branch2, lr2, branch3, lr3, t) = caseBranches addPrec (coerce1 (split e2)) (coerce1 (split e3))
			in
				WithPrec1 (
					bind1 (fn val1 => Eif(val1, branch2, branch3)), 
					addTy t, 
					(PPvar link, chain2 (predResi, Ecase(prec,lr2,lr3)))
				)
			end
		| Flet (e1, (x,e2)) => 
			let
				val x = convertPattern x
			in
				merge2 (split e1, split e2) 
					(fn (bind1, bind2, wrap) => bind1 (fn e1 => Elet (e1, (x, bind2 wrap))))
					(fn (r1,r2) => Elet(r1,(x,r2)))
			end
		| Fbinop (bo,e1,e2) =>
			simpleMerge2 (split e1, split e2) (fn (a,b) => Ebinop(bo,a,b)) (fn (r1,r2) => chain3(r1,r2,Etuple[]))
		| Froll (_,e) => merge1 (split e) (fn (bind,wrap) => bind (wrap o Eroll)) Eroll
		| Funroll e => merge1 (split e) (fn (bind,wrap) => bind (wrap o Eunroll)) Eunroll
		| Ferror t => NoPrec1 (Eerror (firstImage t), Eerror (secondImage t))
	end
  | stageSplit1 (E1next e) = (
		case stageSplit2 e of
		  NoPrec2 r => NoPrec1 (Eunit, r)
		| WithPrec2 (p,t,b) => WithPrec1 (Etuple [Eunit, p],t,b))
  | stageSplit1 (E1hold e) =
		let
			val (link, pi) = freshPi ()
		in
			case stageSplit1 e of
			  NoPrec1 (i, r) => WithPrec1 (Etuple [Eunit,i], Tint, (PPvar link, chain2 (r, Evar link)))
			| WithPrec1 (c, tb, lr) =>
				WithPrec1 (Etuple [Eunit,c], Tprod [Tint,tb], (PPvar link, chain2 (bind (pi 1) lr, pi 0)))
		end

and stageSplit2 (E2 exp) = 
	let
		val split = stageSplit2
		fun merge1 (NoPrec2 r) f = NoPrec2 (f r)
		  | merge1 (WithPrec2 (c,b,(l,r))) f = WithPrec2 (c,b,(l,f r))
		fun merge2 (NoPrec2 e1, res) f = merge1 res (fn x => f (e1, x))
		  | merge2 (res, NoPrec2 e2) f = merge1 res (fn x => f (x, e2))
		  | merge2 (WithPrec2 (p1,t1,(l1,r1)), WithPrec2 (p2,t2,(l2,r2))) f =
					WithPrec2 (Etuple [p1,p2], Tprod [t1,t2], (PPtuple[l1,l2], f (r1, r2)))
		fun merge3 (NoPrec2 e1, res2, res3) f = merge2 (res2, res3) (fn (x,y) => f (e1, x, y))
		  | merge3 (res1, NoPrec2 e2, res3) f = merge2 (res1, res3) (fn (x,y) => f (x, e2, y))
		  | merge3 (res1, res2, NoPrec2 e3) f = merge2 (res1, res2) (fn (x,y) => f (x, y, e3))
		  | merge3 (WithPrec2 (p1,t1,(l1,r1)), WithPrec2 (p2,t2,(l2,r2)), WithPrec2 (p3,t3,(l3,r3))) f = 
					WithPrec2 (Etuple [p1,p2,p3], Tprod [t1,t2,t3], (PPtuple [l1,l2,l3], f (r1, r2, r3)))
	in
		case exp of 
		  Fvar v => NoPrec2 (Evar v)
		| Funit => NoPrec2 Eunit
		| Fint i => NoPrec2 (Eint i)
		| Fbool b => NoPrec2 (Ebool b)
		| Flam (t, (x,e)) => merge1 (split e) (fn r => Elam (trType2 t, (convertPattern x,r)))
		| Fapp (e1, e2) => merge2 (split e1, split e2) (fn (a,b) => Eapp (a,b))
		| Ftuple (e1, e2) => merge2 (split e1, split e2) (fn (a,b) => Etuple [a,b])
		| Fpi (lr, e) => merge1 (split e) (fn r => Epi (index lr, r))
		| Finj (lr, t, e) => merge1 (split e) (fn r => Einj (lr, trType2 t, r))
		| Fif (e1, e2, e3) => merge3 (split e1, split e2, split e3) (fn (a,b,c) => Eif (a,b,c))
		| Fcase (e1,(x2,e2),(x3,e3)) => merge3 (split e1, split e2, split e3) 
			(fn (a,b,c) => Ecase (a,(convertPattern x2,b),(convertPattern x3,c)))
		| Fbinop (bo,e1,e2) => merge2 (split e1, split e2) (fn (a,b) => Ebinop(bo,a,b))
		| Flet (e1,(x, e2)) => merge2 (split e1, split e2) (fn (a,b) => Elet(a,(convertPattern x,b)))
		| Froll (_,e) => merge1 (split e) Eroll
		| Funroll e => merge1 (split e) Eunroll
		| Ferror t => NoPrec2 (Eerror (trType2 t))
	end
  | stageSplit2 (E2prev e) = (
		case stageSplit1 e of
		  NoPrec1 (Etuple [], r) => NoPrec2 r
		| NoPrec1 (Evar _, r) => NoPrec2 r
		| NoPrec1 (c,r) => WithPrec2 (chain2 (c,Eunit), Tprod [], (PPtuple [], r))
		| WithPrec1 (c, tb, lr) => WithPrec2 (Epi (1, c),tb,lr))
end
