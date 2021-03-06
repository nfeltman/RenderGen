
structure StageSplit = 
struct


local

open LangCommon
open LambdaPSF
open Lambda12
structure S = SourceLang
structure T = TypesBase

infixr 9 `
fun a ` b = a b

val PPtuple = LambdaPSF.P o S.Ptuple
val PPvar = LambdaPSF.P o S.Pvar

fun convertMonoPattern (Lambda12.P p) = LambdaPSF.P ` S.mapPattern convertMonoPattern p
  | convertMonoPattern _ = raise InvariantViolation
fun splitPattern1 (Lambda12.P p) = LambdaPSF.P ` S.mapPattern splitPattern1 p
  | splitPattern1 (Lambda12.Pmono p) = convertMonoPattern p
  | splitPattern1 (Lambda12.Pnext p) = LambdaPSF.P (S.Ptuple [])
fun splitPattern2 (Lambda12.P (S.Proll p)) = splitPattern2 p
  | splitPattern2 (Lambda12.P p) = LambdaPSF.P ` S.mapPattern splitPattern2 p
  | splitPattern2 (Lambda12.Pmono p) = LambdaPSF.P (S.Ptuple [])
  | splitPattern2 (Lambda12.Pnext p) = convertMonoPattern p
fun splitPattern x = (splitPattern1 x, splitPattern2 x)
  
val Eunit = Etuple []

fun freshPi () = 
	let
		val l = Variable.newvar "l"
	in
		(l, fn i => Epi (i, Evar l))
	end

fun terminates (E e) = case e of
    S.Fapp _ => false
  | S.SEdata (S.Eerror _) => false
  | other => List.all id ` S.collectExpr ` S.mapExpr terminates id id other 
 
fun unusedAnswer (e as E exp) = case exp of
	S.Fvar _ => []
  | S.SEdata (S.EprimVal _) => []
  | S.Flam _ => []
  | S.Fapp (e1,e2) => [e]
  | S.SEprod e => List.concat ` S.collectProdExpr ` S.mapProdExpr unusedAnswer e
  | S.SEdata (S.Einj (_,_,e)) => unusedAnswer e
  | S.SEdata (S.Ecase (e1,bs)) => [E (S.SEdata (S.Ecase (e1,map (fn (x,e) => (x,chain2 (e, Eunit))) bs)))]
  | S.SEdata (S.Eif (e1,e2,e3)) => [E (S.SEdata (S.Eif (e1,chain2 (e2, Eunit),chain2 (e3, Eunit))))]
  | S.Flet (e1,(x,e2)) => [E (S.Flet (e1,(x,chain2 (e2, Eunit))))]
  | S.SEdata (S.Ebinop (_,e1,e2)) => (unusedAnswer e1) @ (unusedAnswer e2)
  | S.Froll (_,e) => unusedAnswer e
  | S.Funroll e => unusedAnswer e
  | S.Ffix _ => []
  | S.SEdata (S.Eerror _) => [e]

and chain2 (e1,e2) = 
	let
		val es = unusedAnswer e1
		val len = List.length es
	in
		if len = 0 then e2 else Epi (len, Etuple (es@[e2]))
	end
fun chain3 (e1,e2,e3) = 
	case (terminates e1, terminates e2) of
	  (true,  true ) => e3
	| (true,  false) => Epi (1, Etuple [e2,e3])
	| (false, true ) => Epi (1, Etuple [e1,e3])
	| (false, false) => Epi (2, Etuple [e1,e2,e3])
in

type ppatt	= LambdaPSF.pattern
datatype stage1Part			= Splittable of (ppatt * expr) list * expr * expr
							| Opaque of expr
datatype 's1 splitResult1	= NoPrec1 of expr * expr 
							| WithPrec1 of 's1 * (ppatt * expr)
datatype splitResult2 		= NoPrec2 of expr | WithPrec2 of expr * (ppatt * expr)


local

fun flattenContext c b = foldr (fn ((x,e1),e2) => Elet (e1, (x,e2))) b c
fun flattenCVP c v p = flattenContext c ` Etuple [v,p]

fun toOpaque (Opaque e) = e
  | toOpaque (Splittable (cont, v, p)) = flattenCVP cont v p
  
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

fun coercePrec (WithPrec1 (c, lr)) = (c, lr)
  | coercePrec (NoPrec1 (e1,e2)) = (Splittable ([], e1, Eunit), (PPtuple [], e2))

fun merge1 (NoPrec1 (v,r)) f g = NoPrec1 (f v, g r)
  | merge1 (WithPrec1 (e,(l,r))) f g = 
		case toSplit e of (c,v,p) => WithPrec1 (Splittable (c,f v,p),(l, g r))

fun mapResumer1 f (NoPrec1 (v, r)) = NoPrec1 (v, f r)
  | mapResumer1 f (WithPrec1 (a, (l,r))) = WithPrec1 (a, (l,f r))

fun flattenPrecomps pls =
	let
		fun toArray (E (S.SEprod (S.Etuple ps)), LambdaPSF.P (S.Ptuple ls)) = ListPair.zip (ps,ls)
		  | toArray (p,l) = ([(p,l)])
	in
		List.concat ` map toArray pls
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
			let
				val (p,l) = unzip ` flattenPrecomps [(p1,l1), (p2,l2)]
			in
				WithPrec1 (Splittable (List.concat[c1, c2], f (v1,v2), Etuple p), (PPtuple l, g (r1,r2)))
			end

fun simpleMerge results f g = 
		let
			fun h (NoPrec1 (e,r), (cs,vs,pls,rs)) = (cs,e::vs,pls,r::rs)
			  | h (WithPrec1 ((c,v,p), (l,r)), (cs,vs,pls,rs)) = (c::cs, v::vs, (p,l)::pls,r::rs)
			val (cs,vs,pls,rs) = foldr h ([],[],[],[]) (map (mapSplitResult toSplit) results)
		in
			case flattenPrecomps pls of
			  [] => NoPrec1 (f vs, g rs)
			| [(p,l)] => WithPrec1 (Splittable (List.concat cs, f vs, p), (l, g rs))
			| many =>	
				case unzip many of 
				(ps,ls) => WithPrec1 (Splittable (List.concat cs, f vs, Etuple ps), (PPtuple ls, g rs))
		end

fun unpackPredicate (NoPrec1 (e,r)) link = (id,e,r,id,link)
  | unpackPredicate (WithPrec1 (c,(l,r))) link = 
		let
			val (v,p) = (Variable.newvar "v", Variable.newvar "p")
			val c = toOpaque c
		in
			(fn x=> Elet (c,(PPtuple[PPvar v, PPvar p], x)), Evar v, r, fn p2 => Etuple[Evar p,p2], PPtuple[l,link])
		end
  
fun decompTuple (E (S.SEprod (S.Etuple [v,p]))) f = f (v,p)
  | decompTuple c f = 
		let
			val (v,p) = (Variable.newvar "v", Variable.newvar "p")
		in
			Elet (c,(PPtuple[PPvar v, PPvar p], f (Evar v, Evar p)))
		end	
fun caseBranch c addPrec ts us = 
		decompTuple c (fn (v,p) => Etuple[v, addPrec(Einj(ts, us, p))])

fun roll e = Eroll ((),e)
fun eraseTy xs = map (fn _ => ()) xs
in

exception Oops

val coerce1 = coerce

(* assume here that the expression already type-checks *)
fun stageSplit1 gamma (L12core exp) : type12 * stage1Part splitResult1 = 
	let
		val (t,exp) = Typecheck12.Checker1.typeCheckSpecial gamma stageSplit1 exp
		fun split (a,b) = b
		fun getType (a,b) = a
		val answer = 
		case exp of 
		  S.Fvar v  => NoPrec1 (Evar v,  Evar v)
		| S.SEdata (S.EprimVal i) => NoPrec1 (Eprim i,  Eunit)
		| S.Flam (t, (x,e)) => 
			let
				val (c,(l,r)) = coerce (split e)
				val (x1,x2) = splitPattern x
			in
				NoPrec1 (Elam ((), (x1, c)), Elam ((),(PPtuple [x2,l], r)))
			end
		| S.Fapp (e1, e2) => 
			let				
			in
				case (split e1, split e2) of
					  (NoPrec1 (v1,r1), NoPrec1 (v2,r2)) =>
						let 
							val l3 = Variable.newvar "l"
						in
							WithPrec1 (Opaque`Eapp (v1,v2), (PPvar l3,Eapp (r1, Etuple [r2, Evar l3])))
						end
					| (NoPrec1 (v1,r1), WithPrec1 (c2, (l2,r2))) => 
						let
							val (c2,v2,p2) = toSplit c2
							val (v3,p3) = (Variable.newvar "v", Variable.newvar "p")
							val l3 = Variable.newvar "l"
						in
							WithPrec1
							( Splittable 
								(c2 @ [(PPtuple [PPvar v3, PPvar p3], Eapp (v1, v2))],
								Evar v3, Etuple[p2, Evar p3]),
							(PPtuple [l2, PPvar l3], Eapp(r1,Etuple[r2, Evar l3]))
							)
						end
					| (WithPrec1 (c1,(l1,r1)), NoPrec1 (v2,r2)) => 
						let
							val (c1,v1,p1) = toSplit c1
							val (v3,p3) = (Variable.newvar "v", Variable.newvar "p")
							val l3 = Variable.newvar "l"
						in
							WithPrec1
							( Splittable
								(c1 @ [(PPtuple [PPvar v3, PPvar p3],Eapp (v1, v2))],
								Evar v3, Etuple[p1,Evar p3]),
							(PPtuple [l1, PPvar l3], Eapp(r1,Etuple[r2, Evar l3]))
							)
						end
					| (WithPrec1 (c1,(l1,r1)), WithPrec1 (c2,(l2,r2))) => 
						let
							val (c1,v1,p1) = toSplit c1
							val (c2,v2,p2) = toSplit c2
							
							val (v3,p3) = (Variable.newvar "v", Variable.newvar "p")
							val l3 = Variable.newvar "l"
						in
							WithPrec1
							( Splittable 
								( c1 @ c2 @ [(PPtuple [PPvar v3, PPvar p3],Eapp (v1,v2))], 
								Evar v3, Etuple[p1,p2,Evar p3]),
							(PPtuple [l1, l2, PPvar l3], Eapp(r1,Etuple[r2, Evar l3]))
							)
						end
			end
		| S.SEprod (S.Etuple es) => simpleMerge (map split es) Etuple Etuple
		| S.SEprod (S.Epi (i, e)) => 
			let
				fun proj x = Epi (i, x)
			in
				merge1 (split e) proj proj
			end
		| S.SEdata (S.Einj (ts, us, e)) => merge1 (split e) (fn v => Einj (eraseTy ts, eraseTy us, v)) id
		| S.SEdata (S.Ecase (e1, bs)) => 
			let
				val (link,z) = (Variable.newvar "l", Variable.newvar "z")
				val (w1,v1,r1,pWrap,l) = unpackPredicate (split e1) (PPvar link)
				
				fun processBranches [] prefixes = ([],[],[])
				  | processBranches ((x,e)::xes) prefixes = 
					let
						val (x1,x2) = splitPattern x
						val (branches,residuals,suffixes) = processBranches xes (()::prefixes)
						val (c,(l,r)) = coerce (split e)
						val branch = caseBranch c pWrap prefixes suffixes
					in
						((x1,branch)::branches, (l, Elet(Evar z,(x2,r)))::residuals, ()::suffixes)
					end
					
				val (branches, residuals, _) = processBranches bs []
			in
				WithPrec1 (
					Opaque ` w1 (Ecase(v1, branches)),
					(l, Elet (r1, (PPvar z, Ecase(Evar link,residuals))))
				)
			end
		| S.SEdata (S.Eif (e1, e2, e3)) => 
			let
				val link = Variable.newvar "l"
				val (w1,v1,r1,pWrap,l) = unpackPredicate (split e1) (PPvar link)
				val ((c2,lr2),(c3,lr3)) = (coerce (split e2), coerce (split e3))
				val (branch2, branch3) = (caseBranch c2 pWrap [] [()], caseBranch c3 pWrap [()] [])
			in
				WithPrec1(
					Opaque ` w1 (Eif(v1, branch2, branch3)),
					(l, chain2(r1, Ecase(Evar link,[lr2,lr3])))
				)
			end
		| S.Flet (e1, (x,e2)) => 
			let
				val (x1,x2) = splitPattern x
				val (res1,res2) = (split e1, split e2)
				fun makeLet1 a b = Elet (a, (x1,b))
				fun makeLet2 a b = Elet (a, (x2,b))
			in
				case mapSplitResult toOpaque res1 of 
				  NoPrec1 (e1,r1) => (
						case mapSplitResult toOpaque res2 of
						  NoPrec1 (e2, r2) => 
							NoPrec1 (makeLet1 e1 e2, makeLet2 r1 r2)
						| WithPrec1 (e2,(l2,r2)) => 
							WithPrec1 (Opaque ` makeLet1 e1 e2, (l2,makeLet2 r1 r2))
					)
				| WithPrec1 (c1, (l1,r1)) =>
					let
						val y = Variable.newvar "y"
						val pat = PPtuple [x1,PPvar y]
					in
						case mapSplitResult toSplit res2 of
						  NoPrec1 (e2, r2) => 
							WithPrec1 (Opaque ` Elet (c1, (pat, Etuple[e2,Evar y])), (l1, makeLet2 r1 r2))
						| WithPrec1 ((c2,v2,p2),(l2,r2)) => 
							WithPrec1 (Opaque ` Elet (c1, (pat, flattenCVP c2 v2 (Etuple[Evar y,p2]) )), 
								(PPtuple [l1,l2],makeLet2 r1 r2))
					end
			end
		| S.SEdata (S.Ebinop (bo,e1,e2)) =>
			simpleMerge2 (split e1, split e2) (fn (a,b) => Ebinop(bo,a,b)) (fn (r1,r2) => chain3(r1,r2,Etuple[]))
		| S.Froll (_,e) => merge1 (split e) roll id
		| S.Funroll e => merge1 (split e) Eunroll id
		| S.Ffix (_,_,(fx,e)) => 
			let
				val (s,(l,r)) = coercePrec (split e)
				val (cont,v,p) = toSplit s
				val (fx1,fx2) = splitPattern fx
				val (f,x,l0) = (Variable.newvar "f", Variable.newvar "x", Variable.newvar "l")
			in
				NoPrec1(
					Efix ((),(),(fx1, flattenCVP cont v (Eroll ((),p)))),
					Efix ((),(),(PPtuple [PPvar f, PPtuple [PPvar x,PPvar l0]], 
						Elet (Etuple [Etuple [Evar f, Evar x], Eunroll ` Evar l0], 
							(PPtuple [fx2, l], r) 
					)))
				)
			end
		| S.SEdata (S.Eerror t) => NoPrec1 (Eerror (), Eerror ())
	in
		(t,
		case answer of
		  NoPrec1 (e,r) => NoPrec1 (e, r)
		| WithPrec1 (e, (x, r)) => WithPrec1 (e, (x, r)) )
	end
  | stageSplit1 gamma (L12stage expr) =
  	case expr of
	  E1next e =>
		let
			val (t,res) = stageSplit2 gamma e
		in
			(Tfut t,
			case res of
			  NoPrec2 r => NoPrec1 (Eunit, r)
			| WithPrec2 (p,b) => WithPrec1 (Splittable ([], Eunit, p),b)
			)
		end
	| E1hold e =>
		let
			val (link, pi) = freshPi ()
			val (t,res) = (stageSplit1 gamma e)
			val splitAnswer = 
			case mapSplitResult toOpaque res of
			  NoPrec1 (i, r) => WithPrec1 (Splittable ([], Eunit,i), (PPvar link, chain2 (r, Evar link)))
			| WithPrec1 (c, lr) =>
				WithPrec1 (Splittable ([], Eunit, c), (PPvar link, chain2 (Elet (pi 1, lr), pi 0)))
		in
			(Typecheck12.handleHold t,splitAnswer)
		end
	| E1mono e => 
		let
			fun promoteToPSF (L12core e) = E(S.mapExpr promoteToPSF (fn _ => ()) convertMonoPattern e)
			  | promoteToPSF (L12stage _) = raise InvariantViolation
			val ty = Typecheck12.typeCheckM gamma e
		in
			(Tnow ty, NoPrec1(promoteToPSF e, Eunit))
		end
	| E1pushPrim e => 
		let
			val (t, res) = stageSplit1 gamma e
			val newT = Typecheck12.TypeFeatures1.makeprim ` Typecheck12.TypeFeatures1.unprim ` Typecheck12.unnow ` t
		in 
			(newT, res)
		end
	| E1pushSum e => 
		let
			val (t, res) = stageSplit1 gamma e
			val newT = Typecheck12.TypeFeatures1.makesum ` map Tnow ` Typecheck12.TypeFeatures1.unsum ` Typecheck12.unnow ` t
		in
			(newT, res)
		end
	| E2prev _ => raise InvariantViolation

and stageSplit2 gamma (L12core exp) : type12 * splitResult2 = 
	let
		val (t,exp2) = Typecheck12.Checker1.typeCheckSpecial gamma stageSplit2 exp
		fun split (a,b) = b
		fun merge1 (NoPrec2 r) f = NoPrec2 (f r)
		  | merge1 (WithPrec2 (c,(l,r))) f = WithPrec2 (c,(l,f r))
		fun merge2 (NoPrec2 e1, res) f = merge1 res (fn x => f (e1, x))
		  | merge2 (res, NoPrec2 e2) f = merge1 res (fn x => f (x, e2))
		  | merge2 (WithPrec2 (p1,(l1,r1)), WithPrec2 (p2,(l2,r2))) f =
				let 
					val (p,l) = unzip ` flattenPrecomps [(p1,l1), (p2,l2)]
				in
					WithPrec2 (Etuple p, (PPtuple l, f (r1, r2)))
				end
		fun merge3 (NoPrec2 e1, res2, res3) f = merge2 (res2, res3) (fn (x,y) => f (e1, x, y))
		  | merge3 (res1, NoPrec2 e2, res3) f = merge2 (res1, res3) (fn (x,y) => f (x, e2, y))
		  | merge3 (res1, res2, NoPrec2 e3) f = merge2 (res1, res2) (fn (x,y) => f (x, y, e3))
		  | merge3 (WithPrec2 (p1,(l1,r1)), WithPrec2 (p2,(l2,r2)), WithPrec2 (p3,(l3,r3))) f = 
				let 
					val (p,l) = unzip ` flattenPrecomps [(p1,l1), (p2,l2), (p3,l3)]
				in
					WithPrec2 (Etuple p, (PPtuple l, f (r1, r2, r3)))
				end
				
		fun h (NoPrec2 r, (pls,rs)) = (pls,r::rs)
		  | h (WithPrec2 (p, (l,r)), (pls,rs)) = ((p,l)::pls,r::rs)	
		fun finalize f (pls,rs) = (
			case flattenPrecomps pls of
			  [] => NoPrec2 (f rs)
			| [(p,l)] => WithPrec2 (p, (l, f rs))
			| many => case unzip many of (ps,ls) => WithPrec2 (Etuple ps, (PPtuple ls, f rs)) )
		
		fun mergeList results f = finalize f (foldr h ([],[]) results)
	in
		(t,
		case exp2 of 
		  S.Fvar v => NoPrec2 (Evar v)
		| S.SEdata (S.EprimVal p) => NoPrec2 (Eprim p)
		| S.Flam (t, (x,e)) => merge1 (split e) (fn r => Elam ((), (convertMonoPattern x,r)))
		| S.Fapp (e1, e2) => merge2 (split e1, split e2) (fn (a,b) => Eapp (a,b))
		| S.SEprod (S.Etuple es) => mergeList (map split es) Etuple
		| S.SEprod (S.Epi (i, e)) => merge1 (split e) (fn r => Epi (i, r))
		| S.SEdata (S.Einj (ts, us, e)) => merge1 (split e) (fn r => Einj (eraseTy ts, eraseTy us, r))
		| S.SEdata (S.Eif (e1, e2, e3)) => merge3 (split e1, split e2, split e3) (fn (a,b,c) => Eif (a,b,c))
		| S.SEdata (S.Ecase (e,bs)) => 
			let
				val (xs, es) = unzip bs
				val xs = map convertMonoPattern xs
				fun f (r::rs) = Ecase (r, zip id xs rs Oops)
				  | f [] = raise Oops
			in
				mergeList (map split (e :: es)) f
			end
		| S.SEdata (S.Ebinop (bo,e1,e2)) => merge2 (split e1, split e2) (fn (a,b) => Ebinop(bo,a,b))
		| S.Flet (e1,(x, e2)) => merge2 (split e1, split e2) (fn (a,b) => Elet(a,(convertMonoPattern x,b)))
		| S.Froll (_,e) => merge1 (split e) roll
		| S.Funroll e => merge1 (split e) Eunroll
		| S.Ffix (_,_,(x,e)) => merge1 (split e) (fn r => Efix ((),(),(convertMonoPattern x,r)))
		| S.SEdata (S.Eerror t) => NoPrec2 (Eerror ())
		)
	end
  | stageSplit2 gamma (L12stage (E2prev e)) = 
		let
			val (t,res) = stageSplit1 gamma e: type12 * stage1Part splitResult1
		in
		(Typecheck12.unfut t,
		case res of
		  NoPrec1 (c,r) => if terminates c then NoPrec2 r else WithPrec2 (chain2 (c,Eunit), (PPtuple [], r))
		| WithPrec1 (Splittable (c,v,p), lr) => WithPrec2 (flattenContext c ` chain2 (v, p),lr)
		| WithPrec1 (Opaque c, lr) => WithPrec2 (Epi (1, c),lr)
		)
		end
  | stageSplit2 gamma (L12stage _) = raise InvariantViolation
end
end
end
