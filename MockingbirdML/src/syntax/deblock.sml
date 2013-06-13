

structure Deblock = 
struct
    open List
    open MbDeblocked
	structure P = MbPointed

    exception NotYetImplemented
    exception InconsistentType
    
	fun trBoundType P.Bray = Tray
	  | trBoundType P.Bbox = Tbox
	
	fun trDomainType base P.Dflat = base
	  | trDomainType base (P.Darray d) = Tarray (trDomainType base d)
	  | trDomainType base (P.Dbounded (t,d)) = Tprod [trBoundType t, trDomainType base d]
	
	fun trType (P.Tgeoms d) = trDomainType (Tarray Tgeom) d
	  | trType (P.Tsamps d) = trDomainType (Tarray (Tprod [Tint, Tray])) d
	  | trType P.Tint = Tint
	  | trType P.Tbool = Tbool
	  | trType (P.Thit d) = trDomainType (Tarray (Tprod [Tint, Thit])) d
	  | trType (P.Tprod p) = Tprod (map trType p) 
	
	fun pint i = Eop0 (P0int i)
	
	fun trRegion (P.Elam (arg, ty, exp)) = Elam (arg, trType ty, trExpr exp)
	
	and trExpr expr = 
		case expr of
		  P.Efold (name, f, z, el) => Efold (name, trRegion f, trExpr z, trExpr el)
		| P.Emap (name, n, f, el) => Emap (name, n, trRegion f, trExpr el)
		| P.Eif (e1, e2, e3) => Eif (trExpr e1, trExpr e2, trExpr e3)
		| P.Elet (sink, source, rest) => Elet (sink, trExpr source, trExpr rest)
		| P.Evar v => Evar v
		| P.Ecall (label, arg) => Eop1 (P1call label, trExpr arg)
		| P.Einj (lr,e) => Einj (lr, trExpr e)
		| P.Etuple es => Etuple (map trExpr es)
		| P.Eproj (i, e) => Eproj (i, trExpr e)
		
		| P.Eisect (g,s) => Eop2 (P2isectBoxRay, trExpr g, trExpr s)
		| P.EsizeGt (arr, i) => Eop2(P2gt, Eop1 (P1getSize, trExpr arr), pint i)
		| P.EallocBottomHits (s) => 
				let
					val kr = Variable.newvar "kr"
					val newRegion = Elam (kr, Tprod [Tint, Tray], 
							Etuple [Eproj(0, Evar kr), Eop0 P0botHit])
				in
					Emap ("hitInit_", 1, newRegion, trExpr s)
				end
		| P.EbreakG (dec, source) => Eop1 (P1breakG dec, trExpr source)
		| P.EbreakS (dec, source) => Eop1 (P1breakS dec, trExpr source)
		| P.EboundG e => Eop1 (P1boundG, trExpr e)
		| P.EboundS e => Eproj(1, Eop2 (P2getElement, trExpr e, pint 0))
		| P.EcloserHits (e1, e2) => 
				let
					val h = Variable.newvar "hs"
					val h1 = Variable.newvar "kh1"
					val newRegion = Elam (h, Tprod[Tprod [Tint, Tray], Tprod [Tint, Tray]],
							Elet (h1, Eproj(0, Evar h),
							Etuple [
								Eproj(0, Evar h1),
								Eop2 (P2closerHit, Eproj(1, Evar h1), Eproj(1, Eproj(1, Evar h)))
							])
						)
				in
					Emap ("closer_", 2, newRegion, Etuple[trExpr e1, trExpr e2])
				end
		| P.EunionHits (h1, h2) => Eop2 (P2unionHits, trExpr h1, trExpr h2)
		| P.Ehit (g,s) => 
				let
					val gsVar = Variable.newvar "gs"
					val hArr = Variable.newvar "h1"
					val sk1 = Variable.newvar "sk1"
				in
					Elet (hArr, Eop1 (P1allocHitsArray, pint 1),
					Elet (sk1, Eop2 (P2getElement, trExpr s, pint 0), 
					Elet (Variable.newvar "dummy", 
						 Eop3 (P3setElement, Evar hArr, pint 0, 
							Etuple [
								Eproj (0,Evar sk1),
								Eop2 (P2hit, Eop2 (P2getElement, trExpr g, pint 0), Eproj (1, Evar sk1))
							]),
					Evar hArr)))
				end
		| P.Eint i => pint i
		| P.EallocEmptyHits => Eop1 (P1allocHitsArray, pint 0)
	
	fun trFunction (P.Func (outType, name, (ty, argname), expr)) =
			Func (trType outType, name, (trType ty, argname), trExpr expr)

	fun translate (main, rest) = (trFunction main, map trFunction rest)
end

