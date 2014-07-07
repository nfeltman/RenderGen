
structure StageSplit = 
struct


local

open LangCommon
open LambdaPSF
open Lambda12
structure S = SourceLang

infixr 9 `
fun a ` b = a b

type expr = unit LambdaPSF.expr

fun convertPattern (S.Pvar v) = PPvar v
  | convertPattern (S.Ptuple ps) = PPtuple (map convertPattern ps)
  
val Eunit = Etuple []

fun freshPi () = 
	let
		val l = Variable.newvar "l"
	in
		(l, fn i => Epi (i, Evar l))
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

in

datatype stage1Part			= Splittable of (ppatt * expr) list * expr * expr
							| Opaque of expr
datatype 's1 splitResult1	= NoPrec1 of expr * expr 
							| WithPrec1 of 's1 * (ppatt * expr)
datatype splitResult2 		= NoPrec2 of expr | WithPrec2 of expr * (ppatt * expr)


local

fun flattenContext c b = foldr (fn ((x,e1),e2) => Elet (e1, (x,e2))) b c

fun toOpaque (Opaque e) = e
  | toOpaque (Splittable (cont, v, p)) = flattenContext cont (Etuple [v,p])
  
fun toSplit (Opaque e) = 
		let
			val (v,p) = (Variable.newvar "v", Variable.newvar "p")
		in
			([(PPtuple[PPvar v, PPvar p], e)], Evar v, Evar p)
		end
  | toSplit (Splittable s) = s

fun mapSplitResult f (NoPrec1 vr) = NoPrec1 vr
  | mapSplitResult f (WithPrec1 (a,b)) = WithPrec1 (f a,b)

fun coerce (WithPrec1 (c, lr)) = (toOpaque c, lr)
  | coerce (NoPrec1 (e1,e2)) = (Etuple [e1, Eunit], (PPtuple [], e2))

fun merge1 (NoPrec1 (v,r)) f g = NoPrec1 (f v, g r)
  | merge1 (WithPrec1 (Splittable (c, v,p),(l,r))) f g = WithPrec1 (Splittable (c,f v,p),(l, g r))
  | merge1 (WithPrec1 (Opaque c, (l,r))) f g =
		let
			val (v,p) = (Variable.newvar "v", Variable.newvar "p")
		in
			WithPrec1 (Opaque ` Elet (c,(PPtuple[PPvar v, PPvar p], Etuple [f (Evar v), Evar p])), (l,g r))
		end
		
fun simpleMerge2 (res1,res2) f g = 
		case (mapSplitResult toSplit res1, mapSplitResult toSplit res2) of 
		  (NoPrec1 (e1,r1), NoPrec1 (e2,r2)) => 
			NoPrec1 (f (e1,e2), g (r1,r2))
		| (NoPrec1 (e1,r1), WithPrec1 ((c2,v2,p2), (l2,r2))) => 
			WithPrec1 (Splittable (c2, f (e1,v2), p2), (l2, g (r1,r2)))
		| (WithPrec1 ((c1,v1,p1), (l1,r1)), NoPrec1 (e2,r2)) => 
			WithPrec1 (Splittable (c1, f (v1,e2), p1), (l1, g (r1,r2)))
		| (WithPrec1 ((c1,v1,p1), (l1,r1)), WithPrec1 ((c2,v2,p2), (l2,r2))) => 
			WithPrec1 (Splittable (List.concat[c1, c2], f (v1,v2), Etuple[p1,p2]), (PPtuple[l1,l2], g (r1,r2)))

fun simpleMerge results f g = 
		let
			fun h (NoPrec1 (e,r), (cs,vs,pls,rs)) = (cs,e::vs,pls,r::rs)
			  | h (WithPrec1 ((c,v,p), (l,r)), (cs,vs,pls,rs)) = (c::cs, v::vs, (p,l)::pls,r::rs)
			val (cs,vs,pls,rs) = foldr h ([],[],[],[]) (map (mapSplitResult toSplit) results)
		in
			case pls of
			  [] => NoPrec1 (f vs, g rs)
			| [(p,l)] => WithPrec1 (Splittable (List.concat cs, f vs, p), (l, g rs))
			| many =>	
				case unzip many of 
				(ps,ls) => WithPrec1 (Splittable (List.concat cs, f vs, Etuple ps), (PPtuple ls, g rs))
		end

fun unpackPredicate (NoPrec1 (e,r)) link = (id,e,r,id,link)
(*  | unpackPredicate (Full1 (Etuple [v,p],(l,r))) link = 
		let
			val pvar = Variable.newvar "p"
		in
			(fn x=> Elet (p,(PPvar pvar, x)), v, r, fn p2 => Etuple[Evar pvar,p2], PPtuple[l,link])
		end *)
  | unpackPredicate (WithPrec1 (c,(l,r))) link = 
		let
			val (v,p) = (Variable.newvar "v", Variable.newvar "p")
			val c = toOpaque c
		in
			(fn x=> Elet (c,(PPtuple[PPvar v, PPvar p], x)), Evar v, r, fn p2 => Etuple[Evar p,p2], PPtuple[l,link])
		end
  
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

val coerce1 = coerce  

(* assume here that the expression already type-checks *)
fun stageSplit1 (E1 exp) = 
	let
		val split = stageSplit1 : expr1 -> stage1Part splitResult1
		
	in
		case exp of 
		  S.Fvar v  => NoPrec1 (Evar v,  Evar v)
		| S.Funit   => NoPrec1 (Eunit,   Eunit)
		| S.Fint i  => NoPrec1 (Eint i,  Eunit)
		| S.Fbool b => NoPrec1 (Ebool b, Eunit)
		| S.Flam (t, (x,e)) => 
			let
				val (c,(l,r)) = coerce1 (split e)
			in
				NoPrec1 (Elam ((), (convertPattern x, c)), Elam ((),(PPtuple[convertPattern x,l], r)))
			end
		| S.Fapp (e1, e2) => 
			let
				val (link, pi) = freshPi ()					
			in
				case (split e1, split e2) of
					  (NoPrec1 (v1,r1), NoPrec1 (v2,r2)) =>
						let 
							val link = Variable.newvar "l"
						in
							WithPrec1 (Opaque`Eapp (v1,v2), (PPvar link,Eapp (r1, Etuple [r2, Evar link])))
						end
					| (res1, res2) => 
						let
							val ((c1,lr1),(c2,lr2)) = (coerce1 res1, coerce1 res2) 
							val (v1,p1) = (Variable.newvar "v", Variable.newvar "p")
							val (v2,p2) = (Variable.newvar "v", Variable.newvar "p")
							val (v3,p3) = (Variable.newvar "v", Variable.newvar "p")
						in
							WithPrec1
							( Opaque `
							Elet (Etuple[c1,c2], (PPtuple [PPtuple[PPvar v1, PPvar p1], PPtuple[PPvar v2, PPvar p2]],
								Elet (Eapp (Evar v1,Evar v2), (PPtuple [PPvar v3, PPvar p3], 
									Etuple[Evar v3,Etuple[Evar p1,Evar p2,Evar p3]])))),
							(PPvar link,Eapp(Elet (pi 0, lr1),Etuple[Elet (pi 1, lr2), pi 2]))
							)
						end
			end
		| S.Ftuple es => simpleMerge (map split es) Etuple Etuple
		| S.Fpi (i, e) => 
			let
				fun proj x = Epi (i, x)
			in
				merge1 (split e) proj proj
			end
		| S.Finj (lr, t, e) => merge1 (split e) (fn v => Einj (lr,(),v)) id
		| S.Fcase (e1, (x2,e2), (x3,e3)) => 
			let
				val (link,z) = (Variable.newvar "l", Variable.newvar "z")
				val (w1,v1,r1,pWrap,l) = unpackPredicate (split e1) (PPvar link)
				val (branch2, (l2,r2), branch3, (l3,r3)) = caseBranches pWrap (coerce1 (split e2)) (coerce1 (split e3))
				val (x2, x3) = (convertPattern x2, convertPattern x3)
			in
				WithPrec1 (
					Opaque ` w1 (Ecase(v1, (x2,branch2), (x3,branch3))),
					(l, Elet (r1, (PPvar z, Ecase(Evar link,(l2, Elet(Evar z,(x2,r2))),(l3,Elet(Evar z,(x3,r3)))))))
				)
			end
		| S.Fif (e1, e2, e3) => 
			let
				val link = Variable.newvar "l"
				val (w1,v1,r1,pWrap,l) = unpackPredicate (split e1) (PPvar link)
				val (branch2, lr2, branch3, lr3) = caseBranches pWrap (coerce1 (split e2)) (coerce1 (split e3))
			in
				WithPrec1(
					Opaque ` w1 (Eif(v1, branch2, branch3)),
					(l, chain2(r1, Ecase(Evar link,lr2,lr3)))
				)
			end
		| S.Flet (e1, (x,e2)) => 
			let
				val x = convertPattern x
				fun makeLet e1 e2 = Elet (e1, (x,e2))
				val (res1,res2) = (split e1, split e2)
			in
				case mapSplitResult toOpaque res1 of 
				  NoPrec1 (e1,r1) => (
						case mapSplitResult toSplit res2 of
						  NoPrec1 (e2, r2) => 
							NoPrec1 (makeLet e1 e2, makeLet r1 r2)
						| WithPrec1 ((c2,v2,p2),(l2,r2)) => 
							WithPrec1 (Opaque ` makeLet e1 ` flattenContext c2 ` Etuple [v2,p2], (l2,makeLet r1 r2))
					)
				| WithPrec1 (c1, (l1,r1)) =>
					let
						val y = Variable.newvar "y"
						val pat = PPtuple [x,PPvar y]
					in
						case mapSplitResult toSplit res2 of
						  NoPrec1 (e2, r2) => 
							WithPrec1 (Opaque ` Elet (c1, (pat, Etuple[e2,Evar y])), (l1,makeLet r1 r2))
						| WithPrec1 ((c2,v2,p2),(l2,r2)) => 
							WithPrec1 (Opaque ` Elet (c1, (pat, flattenContext c2 ` Etuple [v2,Etuple[Evar y,p2]])), (PPtuple[l1,l2],makeLet r1 r2))
					end
			end
		| S.Fbinop (bo,e1,e2) =>
			simpleMerge2 (split e1, split e2) (fn (a,b) => Ebinop(bo,a,b)) (fn (r1,r2) => chain3(r1,r2,Etuple[]))
		| S.Froll (_,e) => merge1 (split e) Eroll Eroll
		| S.Funroll e => merge1 (split e) Eunroll Eunroll
		| S.Ferror t => NoPrec1 (Eerror (), Eerror ())
	end
  | stageSplit1 (E1next e) = (
		case stageSplit2 e of
		  NoPrec2 r => NoPrec1 (Eunit, r)
		| WithPrec2 (p,b) => WithPrec1 (Splittable ([], Eunit, p),b))
  | stageSplit1 (E1hold e) =
		let
			val (link, pi) = freshPi ()
		in
			case mapSplitResult toOpaque ` stageSplit1 e of
			  NoPrec1 (i, r) => WithPrec1 (Splittable ([], Eunit,i), (PPvar link, chain2 (r, Evar link)))
			| WithPrec1 (c, lr) =>
				WithPrec1 (Splittable ([], Eunit, c), (PPvar link, chain2 (Elet (pi 1, lr), pi 0)))
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
					
		fun mergeList results f = 
		let
			fun h (NoPrec2 r, (pls,rs)) = (pls,r::rs)
			  | h (WithPrec2 (p, (l,r)), (pls,rs)) = ((p,l)::pls,r::rs)
			val (pls,rs) = foldr h ([],[]) results
		in
			case pls of
			  [] => NoPrec2 (f rs)
			| [(p,l)] => WithPrec2 (p, (l, f rs))
			| many => case unzip many of (ps,ls) => WithPrec2 (Etuple ps, (PPtuple ls, f rs))
		end
	in
		case exp of 
		  S.Fvar v => NoPrec2 (Evar v)
		| S.Funit => NoPrec2 Eunit
		| S.Fint i => NoPrec2 (Eint i)
		| S.Fbool b => NoPrec2 (Ebool b)
		| S.Flam (t, (x,e)) => merge1 (split e) (fn r => Elam ((), (convertPattern x,r)))
		| S.Fapp (e1, e2) => merge2 (split e1, split e2) (fn (a,b) => Eapp (a,b))
		| S.Ftuple es => mergeList (map split es) Etuple
		| S.Fpi (i, e) => merge1 (split e) (fn r => Epi (i, r))
		| S.Finj (lr, t, e) => merge1 (split e) (fn r => Einj (lr, (), r))
		| S.Fif (e1, e2, e3) => merge3 (split e1, split e2, split e3) (fn (a,b,c) => Eif (a,b,c))
		| S.Fcase (e1,(x2,e2),(x3,e3)) => merge3 (split e1, split e2, split e3) 
			(fn (a,b,c) => Ecase (a,(convertPattern x2,b),(convertPattern x3,c)))
		| S.Fbinop (bo,e1,e2) => merge2 (split e1, split e2) (fn (a,b) => Ebinop(bo,a,b))
		| S.Flet (e1,(x, e2)) => merge2 (split e1, split e2) (fn (a,b) => Elet(a,(convertPattern x,b)))
		| S.Froll (_,e) => merge1 (split e) Eroll
		| S.Funroll e => merge1 (split e) Eunroll
		| S.Ferror t => NoPrec2 (Eerror ())
	end
  | stageSplit2 (E2prev e) = (
		case stageSplit1 e of
		  NoPrec1 (c,r) => if terminates c then NoPrec2 r else WithPrec2 (chain2 (c,Eunit), (PPtuple [], r))
		| WithPrec1 (Splittable (c,v,p), lr) => WithPrec2 (flattenContext c ` chain2 (v, p),lr)
		| WithPrec1 (Opaque c, lr) => WithPrec2 (Epi (1, c),lr))
end
end
end
