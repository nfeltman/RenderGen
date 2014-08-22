
structure PropStage = 
struct

local
open LangCommon
open Lambda12c
open Lambda12
open SourceLang

fun bind v e1 e2 = Elet (e1,(Pvar v,e2))
fun elabLetRec (f,t1,t2,b,e) = 
	let
		val tY = Tarr(Tvar 0,Tarr(t1,t2))
	in
	bind f 
		(bind "r" 
			(Elam(Trec tY, (Pvar "y", 
				bind f
					(Elam (t1,(Pvar "v", Eapp (Eapp (Eunroll (Evar "y"), Evar "y"), Evar "v"))))
					(Elam (t1,b))
			)))
			(Eapp(Evar "r", Eroll(tY, Evar "r")))
		)
		e
	end
	
exception StagePropException
	
(* let empty = roll (unit + (int * 0)) (inl (int * (mu unit + int * 0)) ()) *) 
fun elabDataType stage (ty,cts,e) = 
	let
		fun nameToZero (Tstandard t) = Tstandard (mapType nameToZero t)
		  | nameToZero (Tfut t) = Tfut (nameToZero t)
		  | nameToZero (Tref s) = if s = ty then Tstandard (TFvar 0) else Tref s
		fun process ((c,t)::cts) =
			let
				val (ntpsList,types) = process cts
				val adjustedPrefixes = map (fn (n,ty,p,s) => (n,ty,t::p,s)) ntpsList
			in
				((c,t,[],types)::adjustedPrefixes,t::types)
			end
		  | process [] = ([],[])
		val (ntpsList,types) = process cts
		val openSumType = Tstandard (TFsum (map nameToZero types))
		fun buildInjector ty prefixes suffixes = Elam (ty,(Pvar "x",Eroll(openSumType,Estandard (Finj (prefixes, suffixes, Evar "x")))))
	in
		Eletty(
			stage, 
			ty, 
			Tstandard (TFrec openSumType), 
			List.foldr (fn ((n,t,p,s),rest) => Elet (buildInjector t p s, (Pvar n,rest))) e ntpsList
		)
	end

in

fun id x = x

fun propTy1 D (Tstandard t) = T1 (mapType (propTy1 D) t)
  | propTy1 D (Tfut t) = T1fut (propTy2 D t)
  | propTy1 (D1,D2) (Tref x) = lookup D1 x
  
and propTy2 D (Tstandard t) = T2 (mapType (propTy2 D) t)
  | propTy2 D (Tfut t) = raise StagePropException
  | propTy2 (D1,D2) (Tref x) = lookup D2 x

and prop1r D G (Estandard exp) = 
		E1 (replaceVars (prop1r D) G Variable.newvar (mapExpr id (propTy1 D) exp))
  | prop1r (D1,D2) G (Eletty (StageOne,x,t,e)) = prop1r (extendContext D1 x (propTy1 (D1,D2) t), D2) G e
  | prop1r (D1,D2) G (Eletty (StageTwo,x,t,e)) = prop1r (D1, extendContext D2 x (propTy2 (D1,D2) t)) G e
  | prop1r D G (Eletr args) = prop1r D G (elabLetRec args)
  | prop1r D G (Eletdata args) = prop1r D G (elabDataType StageOne args)
  | prop1r _ _ (Eprev _) = raise StagePropException
  | prop1r D G (Enext e) = E1next (prop2r D G e)
  | prop1r D G (Ehold e) = E1hold (prop1r D G e)

and prop2r D G (Estandard exp) = 
		E2 (replaceVars (prop2r D) G Variable.newvar (mapExpr id (propTy2 D) exp))
  | prop2r (D1,D2) G (Eletty (StageOne,x,t,e)) = prop2r (extendContext D1 x (propTy1 (D1,D2) t), D2) G e
  | prop2r (D1,D2) G (Eletty (StageTwo,x,t,e)) = prop2r (D1, extendContext D2 x (propTy2 (D1,D2) t)) G e
  | prop2r D G (Eletr args) = prop2r D G (elabLetRec args)
  | prop2r D G (Eletdata args) = prop2r D G (elabDataType StageTwo args)
  | prop2r D G (Eprev e) = E2prev (prop1r D G e)
  | prop2r _ _ (Enext _) = raise StagePropException
  | prop2r _ _ (Ehold _) = raise StagePropException
  
val prop1 = prop1r (empty,empty) empty
val prop2 = prop2r (empty,empty) empty

end
	
end
