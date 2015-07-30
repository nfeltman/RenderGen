
structure PropStage = 
struct

local

infixr 9 `
fun a ` b = a b

open LangCommon
structure C = Lambda12c
open Lambda12
open SourceLang
open TypesBase

datatype 'v il1	= IL1standard of ('v il1,'v,'v patt * 'v il1, C.ty) exprF
				| IL1next of 'v il1
				| IL1prev of 'v il1
				| IL1mono of 'v il1
				| IL1pushSum of 'v il1
				| IL1pushPrim of 'v il1
				| IL1hold of 'v il1
				| IL1letty of C.stage * string * C.ty * 'v il1
and 'v patt = IL1P of ('v, 'v patt) pattern
            | IL1Pmono of 'v patt
            | IL1Pnext of 'v patt

fun Eapp x = IL1standard (Fapp x)
fun Elam x = IL1standard (Flam x)
fun Evar x = IL1standard (Fvar x)
fun Elet x = IL1standard (Flet x)
fun Etuple x = IL1standard (SEprod (BranchlessFrag.Etuple x))
fun Einj x = IL1standard (SEdata (DataFrag.Einj x))
fun Eroll x = IL1standard (Froll x)
fun Eunroll x = IL1standard (Funroll x)
fun Efix x = IL1standard (Ffix x)

fun var v = IL1P (Pvar v)
fun bind v e1 e2 = Elet (e1,(var v,e2))

fun stageLet stage (e1,x,e2) = 
  case stage of 
    C.ThisStage => Elet (e1,(x,e2))
  | C.NextStage => Elet (IL1next e1,(IL1Pnext x,e2))
  | C.MonoStage => Elet (IL1mono e1,(IL1Pmono x,e2))

fun elabLetRec stage (f,t1,t2,(x,body),e) = 
  stageLet stage
    (Efix (t1, t2, (IL1P (Ptuple [var f, x]), body)), var f, e)
	
fun elabDataType stage (ty,cts,e) = 
	let
		fun nameToZero (C.Tstandard t) = C.Tstandard (mapType nameToZero t)
		  | nameToZero (C.Tfut t) = C.Tfut t
		  | nameToZero (C.Tnow t) = C.Tnow t
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
				  SOME ty => Elam (ty,(var "x",Eroll(openSumType,Einj (prefixes, suffixes, Evar "x"))))
				| NONE => Eroll(openSumType, Einj (prefixes, suffixes, Etuple []))
	in
		IL1letty(
			stage, 
			ty, 
			C.Tstandard (TFrec openSumType), 
			List.foldr (fn ((n,t,p,s),rest) => stageLet stage (buildInjector t p s, var n, rest)) e ntpsList
		)
	end
fun id x = x
exception ElaborationException
exception StagePropException

fun elabPatt (Lambda12c.P p) = IL1P (mapPattern elabPatt p)
  | elabPatt (Lambda12c.Pmono p) = IL1Pmono (elabPatt p)
  | elabPatt (Lambda12c.Pnext p) = IL1Pnext (elabPatt p)

fun elab (C.Estandard exp) = IL1standard (mapExpr elab id elabPatt exp)
  | elab (C.Eletdecs (dec,e)) = elabDecs C.ThisStage dec (elab e)
  | elab (C.Eprev e) = IL1prev (elab e)
  | elab (C.Emono e) = IL1mono (elab e)
  | elab (C.Enext e) = IL1next (elab e)
  | elab (C.Ehold e) = IL1hold (elab e)
  | elab (C.EpushPrim e) = IL1pushPrim (elab e)
  | elab (C.EpushSum e) = IL1pushSum (elab e)

and elabDecs s [] e = e
  | elabDecs s (dec :: decs) e = 
    case dec of
      C.Dval (x,ebody) => stageLet s (elab ebody, elabPatt x, elabDecs s decs e)
    | C.Dfun (f,t,(x,ebody)) => stageLet s (Elam (t,(elabPatt x, elab ebody)), var f, elabDecs s decs e)
    | C.Dty (x,t) => IL1letty (s,x,t,elabDecs s decs e)
    | C.Drec (f,t1,t2,(x,ebody)) => elabLetRec s (f,t1,t2,(elabPatt x,elab ebody), elabDecs s decs e)
    | C.Ddata (ty,cts) => elabDataType s (ty,cts, elabDecs s decs e)
    | C.Dnext ndecs => elabDecs C.NextStage ndecs (elabDecs s decs e)
    | C.Dmono mdecs => elabDecs C.MonoStage mdecs (elabDecs s decs e)

structure Cont = BasicContext (ListDict (type var = string)) (type t = var)
fun recast G (IL1P p) = let val (p,G) = recastPattern (recast,Cont.extend,Variable.newvar) G p in (IL1P p, G) end
  | recast G (IL1Pmono p) = let val (p,G) = recast G p in (IL1Pmono p, G) end
  | recast G (IL1Pnext p) = let val (p,G) = recast G p in (IL1Pnext p, G) end
  
fun fixVars G (IL1standard exp) = IL1standard (replaceVars fixVars G (recast,Cont.lookup) exp)
  | fixVars G (IL1letty (s,x,t,e)) = IL1letty (s,x,t,fixVars G e)
  | fixVars G (IL1prev e) = IL1prev (fixVars G e)
  | fixVars G (IL1mono e) = IL1mono (fixVars G e)
  | fixVars G (IL1next e) = IL1next (fixVars G e)
  | fixVars G (IL1hold e) = IL1hold (fixVars G e)
  | fixVars G (IL1pushPrim e) = IL1pushPrim (fixVars G e)
  | fixVars G (IL1pushSum e) = IL1pushSum (fixVars G e)

structure MyContext = BasicContext (ListDict (type var = string)) (type t=type12)

fun propPatt12 (IL1P p) = P (mapPattern propPatt12 p)
  | propPatt12 (IL1Pmono p) = Pmono (propPatt12 p)
  | propPatt12 (IL1Pnext p) = Pnext (propPatt12 p)
  
fun propTy1 D (C.Tstandard t) = Tcore (mapType (propTy1 D) t)
  | propTy1 D (C.Tfut t) = Tfut (propTy1 D t)
  | propTy1 D (C.Tnow t) = Tnow (propTy1 D t)
  | propTy1 D (C.Tref x) = MyContext.lookup D x

and prop1r D (IL1standard exp) = L12core (mapExpr (prop1r D) (propTy1 D) propPatt12 exp)
  | prop1r D (IL1letty (C.ThisStage,x,t,e)) = prop1r (MyContext.extend D x (propTy1 D t)) e
  | prop1r D (IL1letty (C.NextStage,x,t,e)) = prop1r (MyContext.extend D x (propTy1 D t)) e
  | prop1r D (IL1letty (C.MonoStage,x,t,e)) = prop1r (MyContext.extend D x (propTy1 D t)) e
  | prop1r D (IL1prev e) = L12stage ` E2prev (prop1r D e)
  | prop1r D (IL1mono e) = L12stage ` E1mono (prop1r D e)
  | prop1r D (IL1next e) = L12stage ` E1next (prop1r D e)
  | prop1r D (IL1hold e) = L12stage ` E1hold (prop1r D e)
  | prop1r D (IL1pushPrim e) = L12stage ` E1pushPrim (prop1r D e)
  | prop1r D (IL1pushSum e) = L12stage ` E1pushSum (prop1r D e)
  
in
structure Cont = Cont
val prop = (prop1r MyContext.empty) o (fixVars Cont.empty) o elab

end
	
end
