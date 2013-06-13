
structure Pointify = 
struct
	
	open CommonSyntax
	open CommonSyntax.OrderTypes
	structure N = MbNormal
	open MbPointed
	
	exception NotYetImplemented
	exception TypeMismatch
	
	fun trDomainType b N.Tflat = Dflat 
	  | trDomainType b (N.Tarray d) = Darray (trDomainType b d)
	  | trDomainType b (N.Tsum _) =  raise NotYetImplemented
	  | trDomainType b (N.Tbounded d) = Dbounded (b, trDomainType b d)
	  | trDomainType b (N.Tfix _) =  raise NotYetImplemented
	  | trDomainType b (N.Tvar _) =  raise NotYetImplemented
	  
	fun trStageType (GeoSamps (g,s)) = Ttuple (trDomainType Bbox g, trDomainType Bray s)
	  | trStageType (Hits h) = Thit (trDomainType Bbox h)
	  | trStageType (Frags _) = raise NotYetImplemented
	
	
	fun tr ty func input = 
			case (ty,func) of 
			  ((tIn,tOut), N.Emmr (domain, _, e)) => (* assume output is a hit for now *) 
				let
					val gs = Variable.newvar "gs"
					
					val (name,variantMplex, getVariantSize, allocInit, mergeOp, bodyInputType) = 
						case (domain, tIn) of
						  (GeoCase, GeoSamps (Tarray gIn, sIn)) => 
								("mapG_",
								EmultiplexGeom, 
								EgetGeomSize, 
								EallocBottomHits (Evar gs), 
								EcloserHits, 
								GeoSamps (gIn,sIn))
						| (SampCase, GeoSamps (gIn, Tarray sIn)) => 
								("mapS_",
								EmultiplexSamp, 
								EgetSampSize, 
								EallocEmptyHits, 
								EunionHits, 
								GeoSamps (gIn,sIn))
						| (HitCase, _) => raise NotYetImplemented
						| (FragCase, _) => raise NotYetImplemented
						| _ => raise TypeMismatch
					
					(* body *)
					val accumResult = Variable.newvar "accumResult"
					val iter = Variable.newvar "iter"
					val internalAccum = Variable.newvar "inAccum"
					val exBody = tr (bodyInputType, tOut) e (variantMplex (Evar gs, Evar iter))
					val body = Rdef ([Thit Dflat],[(Tint, iter),(Thit Dflat, internalAccum)],
							mergeOp (Evar internalAccum, exBody))
					
				in
					Elet(gs, input,
					EforRange (name, Eint 0, getVariantSize (Evar gs), body, allocInit))
				end
			| (_, N.Ecall (lab, tIn)) => 
				let
					val res = Variable.newvar "call_result"
				in
					Ecall (trStageType tIn, lab, input)
				end
			| (_,N.EsizeCase (GeoCase, i, e1, e2)) => 
				let
					val inputVar = Variable.newvar "gs"
				in
					Elet (inputVar, input, 
					Eif (
						Egt (Eint i, EgetSampSize (Evar inputVar)), 
						tr ty e1 (Evar inputVar), 
						tr ty e2 (Evar inputVar)
					))
				end
			| (_,N.EsizeCase (SampCase, _, _, _)) => raise NotYetImplemented
			| (_,N.EsizeCase (HitCase, _, _, _)) => raise NotYetImplemented
			| (_,N.EsizeCase (FragCase, _, _, _)) => raise NotYetImplemented
			| (_,N.ErememberCase (eLeft, eRight)) => raise NotYetImplemented
			| (_,N.Etest e) => 
				let
					val inputVar = Variable.newvar "gs"
				in
					Elet (inputVar, input, 
					Eif (
						Eisect (Evar inputVar), 
						tr ty e (Evar inputVar),
						EallocBottomHits (Evar inputVar)
					))
				end
			| (_,N.EboundG) => EboundG (input)
			| (_,N.EunboundG) => EunboundG (input)
			| ((_,_),N.EpreBoundGStruct e) => 
				let
					val inputVar = Variable.newvar "gs"
				in
					EpreboundG (input, inputVar, tr ty e (Evar inputVar))
				end
			| (_,N.EmapBuildG (e1,e2)) => 
				let
					val inputVar = Variable.newvar "gs"
				in
					raise NotYetImplemented (*EmapG ("mapBuildG_", input, inputVar, tr ty e2 (Evar inputVar))*)
				end
			| (_,N.EfiltS e) => raise NotYetImplemented
			| (_,N.EbreakG decomp) => EbreakG (decomp, input)
			| (_,N.EbreakS decomp) => EbreakS (decomp, input)
			| (_,N.Einj N.LEFT) => Einj (true, input)
			| (_,N.Einj N.RIGHT) => Einj (false, input)
			| ((GeoSamps (Tflat, Tflat),tOut), N.Ehit) => Ehit (input)
			| (_,N.Eshade) => raise NotYetImplemented
			| (ty,N.EunrollG (_,x,t)) => input
			| (_,N.Eid) => input
			| ((tIn, tOut),N.Echain (tMid,e1,e2)) => tr (tMid, tOut) e2 (tr (tIn, tMid) e1 input)
			| _ => raise TypeMismatch

	fun trFunc (name, ty as (tIn, tOut), e) = 
		let
			val inputVar = Variable.newvar "arg"
		in
			Func (trStageType tOut, name, (trStageType tIn, inputVar), tr ty e (Evar inputVar))
		end
			
	fun translate (main, rest) = (trFunc main, map trFunc rest)
end
