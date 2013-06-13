
structure SubstSingletons = 
struct
	open MbDeblocked
	
	exception UnassignedVariable
	
	fun lookup G v = case Util.find G v of NONE => raise UnassignedVariable | SOME x => x
	
	datatype substEntry	= NoChange 
						| Singleton of variable
						| SplitTuple of variable list
	
	fun substLam G (Elam (x, t, e)) = Elam (x, t, substExpr ((x,NoChange)::G) e)
	and substExpr G expr =
		let
			val s = substExpr G
		in
			case expr of 
			  Efold (name, r, e1, e2) => Efold (name, substLam G r, s e1, s e2)
			| Emap (name, i, r, e) => Emap (name, i, substLam G r, s e)
			| Elet (x, e1, e2) => 
				let
					val tr1 = substExpr G e1
				in
					case tr1 of
					  Evar v => substExpr ((x,Singleton v)::G) e2
					| Etuple es => foldr (fn (e,rest) => Elet (es,) )
					| other => Elet (x, other, substExpr ((x,NoChange)::G) e2)
				end
			| Evar x => Evar (case lookup G x of NoChange => x | Singleton v => v) 
			| Etuple es => Etuple (map s es)
			| Eproj (i, e) => Eproj (i, s e)
			| Einj (b, e) => Einj (b, s e)
			| Eif (e1, e2, e3) => Eif (s e1, s e2, s e3)
			| Eop0 p => Eop0 p
			| Eop1 (p,e) => Eop1 (p, s e)
			| Eop2 (p,e1,e2) => Eop2 (p, s e1, s e2)
			| Eop3 (p,e1,e2,e3) => Eop3 (p, s e1, s e2, s e3)
		end
	
	fun substFunc (Func (tOut,name,(tIn,x),e)) = Func (tOut, name, (tIn, x), substExpr [(x,NoChange)] e)
	fun substProgram (f,fs) = (substFunc f, map substFunc fs)
end
