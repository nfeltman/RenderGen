
structure PropStage = 
struct

local
open LangCommon
structure C = Lambda12c
open Lambda12
open SourceLang
open TypesBase
open Contexts

datatype 'v il1	= IL1standard of ('v il1,'v,C.ty) exprF
				| IL1next of 'v il1
				| IL1prev of 'v il1
				| IL1mono of 'v il1
				| IL1hold of 'v il1
				| IL1letty of C.stage * string * C.ty * 'v il1

fun Eapp x = IL1standard (Fapp x)
fun Elam x = IL1standard (Flam x)
fun Evar x = IL1standard (Fvar x)
fun Elet x = IL1standard (Flet x)
fun Etuple x = IL1standard (Ftuple x)
fun Eroll x = IL1standard (Froll x)
fun Eunroll x = IL1standard (Funroll x)
fun bind v e1 e2 = Elet (e1,(Pvar v,e2))

fun elabLetRec (f,t1,t2,b,e) = 
	let
		val tY = C.Tarr(C.Tvar 0,C.Tarr(t1,t2))
	in
	bind f 
		(bind "r" 
			(Elam(C.Trec tY, (Pvar "y", 
				bind f
					(Elam (t1,(Pvar "v", Eapp (Eapp (Eunroll (Evar "y"), Evar "y"), Evar "v"))))
					(Elam (t1,b))
			)))
			(Eapp(Evar "r", Eroll(tY, Evar "r")))
		)
		e
	end
	
	
fun elabDataType (stage,ty,cts,e) = 
	let
		fun nameToZero (C.Tstandard t) = C.Tstandard (mapType nameToZero t)
		  | nameToZero (C.Tfut t) = C.Tfut (nameToZero t)
		  | nameToZero (C.Tref s) = if s = ty then C.Tstandard (TFvar 0) else C.Tref s
		fun process ((c,tyOpt)::cts) =
			let
				val t = getOpt (tyOpt, C.Tprod [])
				val (ntpsList,types) = process cts
				val adjustedPrefixes = map (fn (n,ty,p,s) => (n,ty,t::p,s)) ntpsList
			in
				((c,tyOpt,[],types)::adjustedPrefixes,t::types)
			end
		  | process [] = ([],[])
		val (ntpsList,types) = process cts
		val openSumType = C.Tstandard (TFsum (map nameToZero types))
		fun buildInjector tyOpt prefixes suffixes =
				case tyOpt of
				  SOME ty => Elam (ty,(Pvar "x",Eroll(openSumType,IL1standard (Finj (prefixes, suffixes, Evar "x")))))
				| NONE => Eroll(openSumType,IL1standard (Finj (prefixes, suffixes, Etuple [])))
		fun stageWrap x = case stage of C.ThisStage => x | C.NextStage => IL1next x
	in
		IL1letty(
			stage, 
			ty, 
			C.Tstandard (TFrec openSumType), 
			List.foldr (fn ((n,t,p,s),rest) => Elet (stageWrap(buildInjector t p s), (Pvar n,rest))) e ntpsList
		)
	end
fun id x = x
exception ElaborationException
exception StagePropException
	
fun elab (C.Estandard exp) = IL1standard (mapExpr elab id exp)
  | elab (C.Eletty (s,x,t,e)) = IL1letty (s,x,t,elab e)
  | elab (C.Eletr (f,t1,t2,(x,ebody),e)) = elabLetRec (f,t1,t2,(x,elab ebody), elab e)
  | elab (C.Eletdata (stage,ty,cts,e)) = elabDataType (stage,ty,cts, elab e)
  | elab (C.Eprev e) = IL1prev (elab e)
  | elab (C.Emono e) = IL1mono (elab e)
  | elab (C.Enext e) = IL1next (elab e)
  | elab (C.Ehold e) = IL1hold (elab e)

fun fixVars G (IL1standard exp) = IL1standard (replaceVars fixVars G Variable.newvar exp)
  | fixVars G (IL1letty (s,x,t,e)) = IL1letty (s,x,t,fixVars G e)
  | fixVars G (IL1prev e) = IL1prev (fixVars G e)
  | fixVars G (IL1mono e) = IL1mono (fixVars G e)
  | fixVars G (IL1next e) = IL1next (fixVars G e)
  | fixVars G (IL1hold e) = IL1hold (fixVars G e)

open TripleContext
  
  
fun propTy1 D (C.Tstandard t) = T1 (mapType (propTy1 D) t)
  | propTy1 D (C.Tfut t) = T1fut (propTy2 D t)
  | propTy1 D (C.Tref x) = lookup1 D x
  
and propTy2 D (C.Tstandard t) = T2 (mapType (propTy2 D) t)
  | propTy2 D (C.Tfut t) = raise StagePropException
  | propTy2 D (C.Tref x) = lookup2 D x
  
and propTyM D (C.Tstandard t) = T2 (mapType (propTyM D) t)
  | propTyM D (C.Tfut t) = raise StagePropException
  | propTyM D (C.Tref x) = lookup3 D x

and prop1r D (IL1standard exp) = E1 (mapExpr (prop1r D) (propTy1 D) exp)
  | prop1r D (IL1letty (C.ThisStage,x,t,e)) = prop1r (extendContext1 D x (propTy1 D t)) e
  | prop1r D (IL1letty (C.NextStage,x,t,e)) = prop1r (extendContext2 D x (propTy2 D t)) e
  | prop1r _ (IL1prev _) = raise StagePropException
  | prop1r D (IL1mono e) = E1mono (propM D e)
  | prop1r D (IL1next e) = E1next (prop2r D e)
  | prop1r D (IL1hold e) = E1hold (prop1r D e)

and prop2r D (IL1standard exp) = E2 (mapExpr (prop2r D) (propTy2 D) exp)
  | prop2r D (IL1letty (C.ThisStage,x,t,e)) = prop2r (extendContext2 D x (propTy2 D t)) e
  | prop2r _ (IL1letty (C.NextStage,x,t,e)) = raise StagePropException
  | prop2r D (IL1prev e) = E2prev (prop1r D e)
  | prop2r _ (IL1mono _) = raise StagePropException
  | prop2r _ (IL1next _) = raise StagePropException
  | prop2r _ (IL1hold _) = raise StagePropException

and propM D (IL1standard exp) = EM (mapExpr (propM D) (propTyM D) exp)
  | propM D (IL1letty (C.ThisStage,x,t,e)) = propM (extendContext3 D x (propTyM D t)) e
  | propM _ (IL1letty (C.NextStage,x,t,e)) = raise StagePropException
  | propM D (IL1prev e) = raise StagePropException
  | propM _ (IL1mono _) = raise StagePropException
  | propM _ (IL1next _) = raise StagePropException
  | propM _ (IL1hold _) = raise StagePropException
  
in
  
val prop1 = (prop1r empty) o (fixVars empty) o elab
val prop2 = (prop2r empty) o (fixVars empty) o elab

end
	
end
