

structure Deblock = 
struct
    open List
    open MbDeblocked
	structure P = MbPointed

    exception NotYetImplemented
    exception InconsistentType
    
	fun trBoundType P.Bray = Tray
	  | trBoundType P.Bbox = Tbox
		
	fun trType P.TgeomsFlat = Tarray Tgeom
	  | trType P.TsampsFlat = Tarray Tray
	  | trType P.Tint = Tint
	  | trType P.Tbool = Tbool
	  | trType P.ThitsFlat = Tarray Thit
	  | trType (P.Tbound b) = trBoundType b
	  | trType (P.Tarray t) = Tarray (trType t)
	  | trType (P.Tprod p) = Tprod (map trType p)
	  | trType (P.Tsum (t1,t2)) = Tsum (trType t1, trType t2)
	  | trType (P.Tfix (v,t)) = Tfix (v, trType t)
	  | trType (P.Tvar v) = Tvar v
	
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
					val newRegion = Elam (Variable.newvar "dummy", Tray, Eop0 P0botHit)
				in
					Emap ("hitInit_", 1, newRegion, trExpr s)
				end
		| P.EbreakG (dec, source) => Eop1 (P1breakG dec, trExpr source)
		| P.EbreakS (dec, source) => Eop1 (P1breakS dec, trExpr source)
		| P.EboundG e => Eop1 (P1boundG, trExpr e)
		| P.EboundS e => Eop2 (P2getElement, trExpr e, pint 0)
		| P.EcloserHits (e1, e2) => 
				let
					val h = Variable.newvar "hs"
					val newRegion = Elam (h, Tprod[Tray, Tray],
								Eop2 (P2closerHit, Eproj(0, Evar h), Eproj(1, Evar h))
							)
				in
					Emap ("closer_", 2, newRegion, Etuple[trExpr e1, trExpr e2])
				end
		| P.EunionHits (h1, h2) => Eop2 (P2unionHits, trExpr h1, trExpr h2)
		| P.Ehit (g,s) => 
				let
					val hArr = Variable.newvar "h1"
				in
					Elet (hArr, Eop1 (P1allocHitsArray, pint 1),
					Elet (Variable.newvar "dummy", 
						 Eop3 (P3setElement, Evar hArr, pint 0, 
							Eop2 (P2hit, 
								Eop2 (P2getElement, trExpr g, pint 0), 
								Eop2 (P2getElement, trExpr s, pint 0))),
					Evar hArr))
				end
		| P.Eint i => pint i
		| P.EallocEmptyHits => Eop1 (P1allocHitsArray, pint 0)
	
	fun trFunction (P.Func (outType, name, (ty, argname), expr)) =
			Func (trType outType, name, (trType ty, argname), trExpr expr)

	fun translate (main, rest) = (trFunction main, map trFunction rest)
end

