
structure PrimitivizeSML =
struct
	open CommonSyntax
	structure P = MbDeblocked
	open MbSML
    
	exception NotImplemented
	exception TypeMismatch
	
	val map = List.map
	
	fun getGBreakFunction OneG = "performOneG"
	  | getGBreakFunction TwoGP = "performTwoGP"
	fun getSBreakFunction OneS = "performOneS"
	  | getSBreakFunction _ = raise NotImplemented
	
	fun trVar var = Variable.toString var
	fun getVar s = trVar (Variable.newvar s)
	
	fun trType P.Tgeom = Tgeom
	  | trType P.Tray = Tsamp
	  | trType P.Tint = Tint
	  | trType P.Thit = Thit
	  | trType P.Tbool = Tbool
	  | trType P.Tbox = Tbox
	  | trType (P.Tarray t) = Tprod [Tint, Tarray (trType t)]
	  | trType (P.Tprod el) = Tprod (map trType el)
	  | trType (P.Tsum (t1,t2)) = Tsum (trType t1, trType t2)
	  | trType (P.Tfix (v,t)) = Tfix (trVar v, trType t)
	  | trType (P.Tvar v) = Tvar (trVar v)
	
	(*
	fun getInhabitant t = 
		case t of
		  Tgeom => Evar "zeroTri"
		| Tsamp => Evar "zeroRay"
		| Tint => Eint 0
		| Thit => Evar "bottomHit"
		| Tbool => Evar ""
		| Tbox => Evar "zeroBox"
		| Tarray u => Ecall ("alloc",Etuple [Eint 0, getInhabitant u])
		| Tprod ts => Etuple (map getInhabitant ts)
		| Tsum (t1,t2) => raise NotImplemented
		| Tfix (v,t) => raise NotImplemented
		| Tvar v => raise NotImplemented *)
	
	fun trExpr expr =
		case expr of 
		  P.Efold (name, P.Elam (arg, ty, exp), init, el) => 
			let
				val ia = getVar "ia"
				val iter = getVar "i"
				val sizelist = getVar "sizelist"
				val size = getVar "size"
				val listv = getVar "list"
				val headerLabel = getVar (name ^ "loop")
				
				val loopfunc = (headerLabel, ia, trType ty,
					Elet (iter, Eproj(0, Evar ia),
					Eif (
						EbinOp (Bgt, Evar size, Evar iter), 
						Ecall (headerLabel, 
							Elet (trVar arg, Etuple [EgetElement (Evar listv, Evar iter), Eproj (1, Evar ia)] , 
							Etuple [EbinOp (Bplus, Evar iter, Eint 1), trExpr exp])),
						Eproj (1, Evar ia)
					)))
			in
				Elet (sizelist, trExpr el,
				Elet (size, Eproj (0, Evar sizelist),
				Elet (listv, Eproj (1, Evar sizelist),
				EfuncDefs (loopfunc, [],
				Ecall (headerLabel, Etuple [Eint 0, trExpr init])))))
			end
		| P.Emap (name, n, P.Elam (arg, ty, exp), el) => 
			let
				val sink = getVar "sink"
				val sinklist = getVar "sinklist"
				val iter = getVar "i"
				val input = getVar "lists"
				val size = getVar "size"
				val headerLabel = getVar (name ^ "loop")
				
				fun genParts 0 getters = Elet (trVar arg, Etuple getters, trExpr exp)
				  | genParts k getters = 
					let
						val listk = getVar ("list" ^ (Int.toString k))
					in
						Elet (listk, Eproj (1, Eproj(k-1, Evar input)), genParts (k-1) (EgetElement (Evar listk, Evar iter) :: getters))
					end
				
				val (parts, sizeExp) = 
						case n of
						  1 => (Elet (trVar arg, EgetElement (Eproj (1, Evar input), Evar iter), trExpr exp),
								Eproj (0, Evar input))
						| k => (genParts k [], Eproj (0, Eproj (0, Evar input)))
				
				val loopfunc = (headerLabel, iter, Tint,
					Eif (
						EbinOp (Bgt, Evar size, Evar iter), 
						Ecall (headerLabel,
							Elet (getVar "dummy", EsetElement (Evar sinklist, Evar iter, parts),
							EbinOp (Bplus, Evar iter, Eint 1))),
						Evar sink
					))
			in
				Elet (input, trExpr el,
				Elet (size, sizeExp,
				Elet (sink, Ecall ("allocHits", Evar size) (*Ecall ("alloc", Etuple [Evar size, getInhabitant (trType ty)])*),
				Elet (sinklist, Eproj (1, Evar sink),
				EfuncDefs (loopfunc, [],
				Ecall (headerLabel, Eint 0))))))
			end
		| P.Elet (sink, v, rest) => Elet (trVar sink, trExpr v, trExpr rest)
		| P.Eif (e1, e2, e3) => Eif (trExpr e1, trExpr e2, trExpr e3)
		| P.Eop0 op0 =>
			(
				case op0 of
				  P.P0int i => Eint i
				| P.P0botHit => EbotHit
			)
		| P.Eop1 (op1, e1) =>
			let
				val tr1 = trExpr e1
			in
				case op1 of
				  P.P1call label => Ecall (trVar label, tr1)
				| P.P1getSize => Eproj (0, tr1)
				| P.P1boundG => Ecall ("boundGeoms", tr1)
				| P.P1allocHitsArray => Ecall ("allocHits", tr1)
				| P.P1breakG (dec) => Ecall (getGBreakFunction dec, tr1)
				| P.P1breakS (dec) => Ecall (getSBreakFunction dec, tr1)
			end
		| P.Eop2 (op2, e1, e2) =>
			let
				val tr1 = trExpr e1
				val tr2 = trExpr e2
			in
				case op2 of
				  P.P2getElement => EgetElement (Eproj (1, tr1), tr2)
				| P.P2unionHits => Ecall ("unionHits", Etuple [tr1, tr2])
				| P.P2hit => Ecall ("isect", Etuple [tr1, tr2])
				| P.P2closerHit => Ecall ("closer", Etuple [tr1, tr2])
				| P.P2isectBoxRay => Ecall ("isectBoxRay", Etuple [tr1, tr2])
				| P.P2plus => EbinOp (Bplus, tr1, tr2)
				| P.P2gt => EbinOp (Bgt, tr1, tr2)
			end
		| P.Eop3 (op3, e1, e2, e3) =>
			let
				val tr1 = trExpr e1
				val tr2 = trExpr e2
				val tr3 = trExpr e3
			in
				case op3 of
				  P.P3setElement => EsetElement (Eproj (1, tr1), tr2, tr3)
			end
		| P.Evar v => Evar (trVar v)
		| P.Eproj (i,e) => Eproj (i, trExpr e)
		| P.Etuple el => Etuple (map trExpr el)
		| P.Einj (lr, e) => Ecall (if lr then "injL" else "injR", trExpr e)
	
	fun trFunction (P.Func (_, name, (ty,var), rootExpr)) = 
			(trVar name, trVar var, trType ty, trExpr rootExpr)
	
	fun translate ((mainFunc as P.Func (_,name,_,_)), rest) = 
		EfuncDefs (trFunction mainFunc, map trFunction rest, Ecall ("runExperiment", Evar (trVar name)))
end 
