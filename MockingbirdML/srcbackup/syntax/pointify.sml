
structure Pointify = 
struct
	
	open CommonSyntax
	open CommonSyntax.OrderTypes
	structure N = MbNormal
	open MbPointed
	
	exception NotYetImplemented
	exception TypeMismatch
	
	fun trStageType (GeoSamps gs) = Tgs gs
	  | trStageType (Hits h) = Thit h
	  | trStageType (Frags _) = raise NotYetImplemented
	
	fun tr ty func c onEmit = 
			case (ty,func) of 
			  ((tIn,tOut), N.Emmr (domain, e, Hits tMid, rest)) => (* assume output is a hit for now *) 
				let
					val iter = Variable.newvar "iter"
					val accum = Variable.newvar "accum"
					val size = Variable.newvar "sizeMax"
					val size2 = Variable.newvar "sizeDecoy"
					val cBody = Variable.newvar "cBody"
					val newSig =  [(trStageType tIn, cBody), (Thit Tflat, accum)]
					
					val (variantMplex, sinkType, getVariantSize, allocInit, mergeOp, bodyInputType) = 
						case (domain, tIn) of
						  (GeoCase, GeoSamps (Tarray gIn, sIn)) => 
								(EmultiplexGeom, gIn, EgetGeomSize, EallocBottomHits, EcloserHits, GeoSamps (gIn,sIn))
						| (SampCase, GeoSamps (gIn, Tarray sIn)) => 
								(EmultiplexSamp, sIn, EgetSampSize, fn (x,_,_,e) => EallocEmptyHits (x,e), fn (x,v1,v2,_,e) => EunionHits (x,v1,v2,e), GeoSamps (gIn,sIn))
						| (HitCase, _) => raise NotYetImplemented
						| (FragCase, _) => raise NotYetImplemented
						| _ => raise TypeMismatch
										
					(* merge routine *)
					fun subEmit c2 = 
						let 
                            val merged = Variable.newvar "merged"
						in 
                            mergeOp (merged, c2, Vvar accum, [(trStageType tIn, cBody)],
                            Econtinue [Vvar cBody, Vvar merged])
						end
					
					
					(* body *)
					val cnext = Variable.newvar "cnext"
					val exBody = tr (bodyInputType, Hits tMid) e (Vvar cnext) subEmit
					val body =	variantMplex (cnext, sinkType, Vvar cBody, Vvar iter, exBody)
					
					(* exit *)
					val exExit = tr (Hits tMid, tOut) rest (Vvar accum) onEmit
					
				in
					getVariantSize (size, c, 
					Eset(size2, Vvar size,
					allocInit (accum, Vvar size2, [(Tint, size)],
					EforRange (iter, Vint 0, Vvar size, newSig, body, [c , Vvar accum], exExit))))
				end
			| (_,N.Eemit) => onEmit c
			| ((_, tOut), N.Ecall (lab, tMid, rest)) => 
				let
					val res = Variable.newvar "call_result"
					val ex = tr (tMid, tOut) rest (Vvar res) onEmit
				in
					Ecall (res, trStageType tOut, lab, c, ex)
				end
			| (_,N.EsizeCase (GeoCase, i, e1, e2, rest)) => raise NotYetImplemented
			| (_,N.EsizeCase (SampCase, _, _, _, _)) => raise NotYetImplemented
			| (_,N.EsizeCase (HitCase, _, _, _, _)) => raise NotYetImplemented
			| (_,N.EsizeCase (FragCase, _, _, _, _)) => raise NotYetImplemented
			| (_,N.ErememberCase (eLeft, eRight, rest)) => raise NotYetImplemented
			| (_,N.Etest e) => raise NotYetImplemented
			| (_,N.EboundG e) => raise NotYetImplemented
			| (_,N.EunboundG e) => raise NotYetImplemented
			| (_,N.EpreBoundGStruct (e1, e2)) => raise NotYetImplemented
			| (_,N.EmapBuildG (e, rest)) => raise NotYetImplemented
			| (_,N.EfiltS e) => raise NotYetImplemented
			| ((GeoSamps (gIn, sIn), tOut), N.EbreakG (decomp, rest)) => 
				let
					val broken = Variable.newvar "broken"
					val ex = tr (GeoSamps (Tarray gIn,sIn),tOut) rest (Vvar broken) onEmit
				in
					EbreakG (broken, decomp, c, ex)
				end
			| ((GeoSamps (gIn, sIn), tOut), N.EbreakS (decomp, rest)) => 
				let
					val broken = Variable.newvar "broken"
					val ex = tr (GeoSamps (gIn, Tarray sIn), tOut) rest (Vvar broken) onEmit
				in
					EbreakS (broken, decomp, c, ex)
				end
			| (_,N.Einj (side, rest)) => raise NotYetImplemented
			| ((GeoSamps (Tflat, Tflat),tOut), N.Ehit rest) => 
				let
					val h = Variable.newvar "h"
					val ex = tr (Hits Tflat, tOut) rest (Vvar h) onEmit
				in
					Ehit (h, c, ex)
				end
				
			| (_,N.Eshade rest) => raise NotYetImplemented
			| (ty,N.EunrollG (_,x,t,rest)) => tr ty rest c onEmit
			| _ => raise TypeMismatch

	fun trFunc (name, ty as (tIn, tOut), e) = 
		let
			val inputVar = Variable.newvar "arg"
		in
			(trStageType tOut, name, (trStageType tIn, inputVar), tr ty e (Vvar inputVar) (fn c => Eret c)) : func
		end
			
	fun translate program = map trFunc program
end
