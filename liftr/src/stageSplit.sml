
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

datatype splitResult1 = NoPrec1 of unit expr * unit expr | WithPrec1 of unit expr * (ppatt * unit expr)
datatype splitResult2 = NoPrec2 of unit expr | WithPrec2 of unit expr * (ppatt * unit expr)
fun coerce1 (WithPrec1 res) = res
  | coerce1 (NoPrec1 (e1,e2)) = (Etuple [e1, Eunit], (PPtuple [], e2))

(* assume here that the expression already type-checks *)
fun stageSplit1 (E1 exp) = 
	let
		val split = stageSplit1 : expr1 -> splitResult1
		fun id x = x
		
		fun addSubterm (NoPrec1 (e,r)) pa = (id,e,r,pa)
		  | addSubterm (WithPrec1 (Etuple [v,p],(l,r))) pa = (id,v,r,(p,l)::pa)
		  | addSubterm (WithPrec1 (c,(l,r))) pa = 
				let
					val (v,p) = (Variable.newvar "v", Variable.newvar "p")
				in
					(fn x=> Elet (c,(PPtuple[PPvar v, PPvar p], x)), Evar v, r, (Evar p,l)::pa)
				end
		
		fun decompPA [] f v r = NoPrec1 (f v, r)
		  | decompPA [(p,l)] f v r = WithPrec1 (f (Etuple [v,p]), (l,r))
		  | decompPA pa f v r = 
				let val (p, l) = unzip pa in WithPrec1 (f (Etuple [v, Etuple p]), (PPtuple l,r)) end
		
		fun merge1 res f g = 
			let 
				val (w,v,r,pa) = addSubterm res [] 
			in
				decompPA pa w (f v) (g r)
			end
			
		fun simpleMerge2 (res1,res2) f g = 
			let 
				val (w1,v1,r1,pa) = addSubterm res1 [] 
				val (w2,v2,r2,pa) = addSubterm res2 pa
			in
				decompPA pa (w1 o w2) (f (v1,v2)) (g (r1,r2))
			end
		
		fun unpackPredicate (NoPrec1 (e,r)) link = (id,e,r,id,link)
		  | unpackPredicate (WithPrec1 (Etuple [v,p],(l,r))) link = 
				let
					val pvar = Variable.newvar "p"
				in
					(fn x=> Elet (p,(PPvar pvar, x)), v, r, fn p2 => Etuple[Evar pvar,p2], PPtuple[l,link])
				end
		  | unpackPredicate (WithPrec1 (c,(l,r))) link = 
				let
					val (v,p) = (Variable.newvar "v", Variable.newvar "p")
				in
					(fn x=> Elet (c,(PPtuple[PPvar v, PPvar p], x)), Evar v, r, fn p2 => Etuple[Evar p,p2], PPtuple[l,link])
				end
		  
		fun makeTup (a,b) = Etuple [a,b]
		
		fun decompTuple (Etuple [v,p]) f = f (v,p)
		  | decompTuple c f = 
				let
					val (v,p) = (Variable.newvar "v", Variable.newvar "p")
				in
					Elet (c,(PPtuple[PPvar v, PPvar p], f (Evar v, Evar p)))
				end	
		fun caseBranch c addPrec side = 
				decompTuple c (fn (v,p) => Etuple[v, addPrec(Einj(side, (), p))])
		fun caseBranches addPrec (c2, b2) (c3, b3) = 
				(caseBranch c2 addPrec Left, b2, caseBranch c3 addPrec Right, b3)
	in
		case exp of 
		  Fvar v  => NoPrec1 (Evar v,  Evar v)
		| Funit   => NoPrec1 (Eunit,   Eunit)
		| Fint i  => NoPrec1 (Eint i,  Eunit)
		| Fbool b => NoPrec1 (Ebool b, Eunit)
		| Flam (t, (x,e)) => 
			let
				val (c,(l,r)) = coerce1 (split e)
			in
				NoPrec1 (Elam ((), (convertPattern x, c)), Elam ((),(PPtuple[convertPattern x,l], r)))
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
							WithPrec1 (Eapp (v1,v2), (PPvar link,Eapp (r1, Etuple [r2, Evar link])))
						end
					| (res1, res2) => 
						let
							val ((c1,lr1),(c2,lr2)) = (coerce1 res1, coerce1 res2) 
							val (v1,p1) = (Variable.newvar "v", Variable.newvar "p")
							val (v2,p2) = (Variable.newvar "v", Variable.newvar "p")
							val (v3,p3) = (Variable.newvar "v", Variable.newvar "p")
						in
							WithPrec1
							(
							Elet (Etuple[c1,c2], (PPtuple [PPtuple[PPvar v1, PPvar p1], PPtuple[PPvar v2, PPvar p2]],
								Elet (Eapp (Evar v1,Evar v2), (PPtuple [PPvar v3, PPvar p3], 
									Etuple[Evar v3,Etuple[Evar p1,Evar p2,Evar p3]])))),
							(PPvar link,Eapp(Elet (pi 0, lr1),Etuple[Elet (pi 1, lr2), pi 2]))
							)
						end
			end
		| Ftuple (e1, e2) => simpleMerge2 (split e1, split e2) makeTup makeTup
		| Fpi (side, e) => 
			let
				fun proj x = Epi (index side, x)
			in
				merge1 (split e) proj proj
			end
		| Finj (lr, t, e) => merge1 (split e) (fn v => Einj (lr,firstImage t,v)) id
		| Fcase (e1, (x2,e2), (x3,e3)) => 
			let
				val (link,z) = (Variable.newvar "l", Variable.newvar "z")
				val (w1,v1,r1,pWrap,l) = unpackPredicate (split e1) (PPvar link)
				val (branch2, (l2,r2), branch3, (l3,r3)) = caseBranches pWrap (coerce1 (split e2)) (coerce1 (split e3))
				val (x2, x3) = (convertPattern x2, convertPattern x3)
			in
				WithPrec1 (
					w1 (Ecase(v1, (x2,branch2), (x3,branch3))),
					(l, Elet (r1, (PPvar z, Ecase(Evar link,(l2, Elet(Evar z,(x2,r2))),(l3,Elet(Evar z,(x3,r3)))))))
				)
			end
		| Fif (e1, e2, e3) => 
			let
				val link = Variable.newvar "l"
				val (w1,v1,r1,pWrap,l) = unpackPredicate (split e1) (PPvar link)
				val (branch2, lr2, branch3, lr3) = caseBranches pWrap (coerce1 (split e2)) (coerce1 (split e3))
			in
				WithPrec1(
					w1 (Eif(v1, branch2, branch3)),
					(l, chain2(r1, Ecase(Evar link,lr2,lr3)))
				)
			end
		| Flet (e1, (x,e2)) => 
			let
				val x = convertPattern x
				
				val (w1,r1,pa) = 
					case split e1 of
					  NoPrec1 (e,r) => (fn z => Elet (e,(x,z)),r,[])
					| WithPrec1 (Etuple [v,p],(l,r)) => (fn z => Elet (v,(x,z)),r,[(p,l)])
					| WithPrec1 (c,(l,r)) =>
						let
							val p = Variable.newvar "p"
						in
							(fn z=> Elet (c,(PPtuple[x, PPvar p], z)), r, [(Evar p,l)])
						end
				
				val (w2,v2,r2,pa) = addSubterm (split e2) pa
			in				
				decompPA pa (w1 o w2) v2 (Elet(r1,(x,r2)))
			end
		| Fbinop (bo,e1,e2) =>
			simpleMerge2 (split e1, split e2) (fn (a,b) => Ebinop(bo,a,b)) (fn (r1,r2) => chain3(r1,r2,Etuple[]))
		| Froll (_,e) => merge1 (split e) Eroll Eroll
		| Funroll e => merge1 (split e) Eunroll Eunroll
		| Ferror t => NoPrec1 (Eerror (firstImage t), Eerror (secondImage t))
	end
  | stageSplit1 (E1next e) = (
		case stageSplit2 e of
		  NoPrec2 r => NoPrec1 (Eunit, r)
		| WithPrec2 (p,b) => WithPrec1 (Etuple [Eunit, p],b))
  | stageSplit1 (E1hold e) =
		let
			val (link, pi) = freshPi ()
		in
			case stageSplit1 e of
			  NoPrec1 (i, r) => WithPrec1 (Etuple [Eunit,i], (PPvar link, chain2 (r, Evar link)))
			| WithPrec1 (c, lr) =>
				WithPrec1 (Etuple [Eunit,c], (PPvar link, chain2 (bind (pi 1) lr, pi 0)))
		end

and stageSplit2 (E2 exp) = 
	let
		val split = stageSplit2
		fun merge1 (NoPrec2 r) f = NoPrec2 (f r)
		  | merge1 (WithPrec2 (c,(l,r))) f = WithPrec2 (c,(l,f r))
		fun merge2 (NoPrec2 e1, res) f = merge1 res (fn x => f (e1, x))
		  | merge2 (res, NoPrec2 e2) f = merge1 res (fn x => f (x, e2))
		  | merge2 (WithPrec2 (p1,(l1,r1)), WithPrec2 (p2,(l2,r2))) f =
					WithPrec2 (Etuple [p1,p2], (PPtuple[l1,l2], f (r1, r2)))
		fun merge3 (NoPrec2 e1, res2, res3) f = merge2 (res2, res3) (fn (x,y) => f (e1, x, y))
		  | merge3 (res1, NoPrec2 e2, res3) f = merge2 (res1, res3) (fn (x,y) => f (x, e2, y))
		  | merge3 (res1, res2, NoPrec2 e3) f = merge2 (res1, res2) (fn (x,y) => f (x, y, e3))
		  | merge3 (WithPrec2 (p1,(l1,r1)), WithPrec2 (p2,(l2,r2)), WithPrec2 (p3,(l3,r3))) f = 
					WithPrec2 (Etuple [p1,p2,p3], (PPtuple [l1,l2,l3], f (r1, r2, r3)))
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
		| NoPrec1 (c,r) => WithPrec2 (chain2 (c,Eunit), (PPtuple [], r))
		| WithPrec1 (c, lr) => WithPrec2 (Epi (1, c),lr))
end
