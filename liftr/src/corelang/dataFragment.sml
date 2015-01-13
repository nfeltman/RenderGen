
signature DataFragTypes = 
sig
	type t
	val makeprim	: Prims.primType -> t
	val makesum		: t list -> t
	val unprim		: t -> Prims.primType
	val unsum		: t -> t list
end
signature DataFragValues = 
sig
	type v
	val makeprim	: Prims.primValue -> v
	val makeinj		: int * v -> v
	val unprim		: v -> Prims.primValue
	val uninj		: v -> int * v
end

structure DataFrag = 
struct local
	open LangCommon
in
	datatype ('e, 'b, 't) exprF	
		= EprimVal of Prims.primValue
		| Ebinop of Prims.binops * 'e * 'e
		| Einj of 't list * 't list * 'e
		| Ecase of 'e * 'b list
		| Eif of 'e * 'e * 'e
		| Eerror of 't

	fun mapExpr (fe,fb,ft) (EprimVal v) = EprimVal v
	  | mapExpr (fe,fb,ft) (Ebinop (bo,e1,e2)) = Ebinop (bo, fe e1, fe e2)
	  | mapExpr (fe,fb,ft) (Einj (tl,tr,e)) = Einj (map ft tl, map ft tr, fe e)
	  | mapExpr	(fe,fb,ft) (Ecase (e,bs)) = Ecase (fe e, map fb bs)
	  | mapExpr	(fe,fb,ft) (Eif (e1,e2,e3)) = Eif (fe e1, fe e2, fe e3)
	  | mapExpr (fe,fb,ft) (Eerror t) = Eerror (ft t)

	fun collectExpr (EprimVal v) = []
	  | collectExpr (Ebinop (bo,e1,e2)) = [e1,e2]
	  | collectExpr (Einj (tl,tr,e)) = [e]
	  | collectExpr	(Ecase (e,bs)) = e :: bs
	  | collectExpr	(Eif (e1,e2,e3)) = [e1,e2,e3]
	  | collectExpr (Eerror t) = []

	functor TC (T : DataFragTypes) (TS : TypeSystem where type ty = T.t) = 
	struct
		fun propDown ex = 
			case ex of 
			  EprimVal pv => EprimVal pv
			| Einj (ts, us, e) => Einj (ts, us, e)
			| Ecase ((t,e),bs) => Ecase ((t,e), zip id (T.unsum t) bs (TypeError "wrong number of branches"))
			| Eif (e1,e2,e3) => Eif (e1,e2,e3)
			| Eerror t => Eerror t
			| Ebinop (bo, e1, e2) => Ebinop (bo, e1, e2)
		fun checkUp ex = 
			let
				fun binSame (a : Prims.primType, b : Prims.primType) (c,d,e) = 
						if (a = c) andalso (b = d) then e else raise (TypeError "binop")
				fun assertSame (a,b) = if TS.teq a b then a else raise (TypeError "branches not same")
				fun assertAllSame [] = raise (TypeError "no branches in case")
				  | assertAllSame (t::[]) = t
				  | assertAllSame (t::ts) = assertSame (t,assertAllSame ts)
			in
				case ex of 
				  EprimVal pv => T.makeprim (Prims.getValType pv)
				| Ebinop (bo, t1, t2) => T.makeprim (binSame (T.unprim t1, T.unprim t2) (Prims.getBinopType bo))
				| Einj (ts, us, t) => T.makesum (ts @ (t :: us))
				| Ecase (e,ts) => assertAllSame ts
				| Eif (t1,t2,t3) => (Prims.assertBool (T.unprim t1); assertSame (t2, t3))
				| Eerror t => t
			end
	end 

	functor Eval (V : DataFragValues) = 
	struct
		fun eval (eval, evalBranch) ex = 
			case ex of 
			  EprimVal pv => V.makeprim pv
			| Ebinop (bo, e1, e2) => V.makeprim (Prims.evalPrim (bo, V.unprim (eval e1), V.unprim (eval e2)))
			| Einj (ts, us, e) => V.makeinj (length ts, eval e)
			| Ecase (e,bs) => (case V.uninj (eval e) of (i, v) => evalBranch v (List.nth (bs,i)))
			| Eif (e1,e2,e3) => eval (if Prims.unbool (V.unprim (eval e1)) then e2 else e3)
			| Eerror t => raise Stuck
	end

end end
