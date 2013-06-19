
structure Normalize = 
struct
	
	open CommonSyntax
	open MbPointed
	structure S = MbOrder
    
	exception TypeMismatch
    exception NotYetImplemented
	
	fun trDomainType flatType b S.Tflat = flatType 
	  | trDomainType flatType b (S.Tarray d) = Tarray (trDomainType flatType b d)
	  | trDomainType flatType b (S.Tsum (d1,d2)) = Tsum (trDomainType flatType b d1, trDomainType flatType b d2)
	  | trDomainType flatType b (S.Tbounded d) = Tprod [Tbound b, trDomainType flatType b d]
	  | trDomainType flatType b (S.Tfix (v,d)) =  Tfix (v, trDomainType flatType b d)
	  | trDomainType flatType b (S.Tvar v) =  Tvar v
	
	fun trType (GeoSamps (d1, d2)) = Tprod [trDomainType TgeomsFlat Bbox d1, trDomainType TsampsFlat Bray d2]
	  | trType (Hits d) = trDomainType ThitsFlat Bbox d
	  | trType (Frags _) = raise NotYetImplemented
	
	fun translate e wholeType =
		let
			val funcs = ref []
			fun addFunc z = (funcs := z :: !funcs)
			
			fun tr eSource carry =
				case ((),eSource) of 
				  (_, S.Echain (e1, e2)) => tr e2 (tr e1 carry)
				| (_, S.Emmr (domain, e)) => 
						let
							val gs = Variable.newvar "gs"
							val const = Variable.newvar "constPart"
							val x = Variable.newvar "x"
							
							val loop = 
								case (domain, ()) of
								  (GeoCase, _) => 
										Elet (const, Eproj (1, Evar gs),
										Efold ("mapG_", 
											Elam (x, Tprod [Tint, ThitsFlat],
												EcloserHits (Eproj(1, Evar x), tr e (Etuple [Eproj(0, Evar x), Evar const]))), 
											EallocBottomHits (Eproj (1, Evar gs)), 
											Eproj (0, Evar gs)))
								| (SampCase, _) => 
										Elet (const, Eproj (0, Evar gs),
										Efold ("mapS_", 
											Elam (x, Tprod [Tint, ThitsFlat],
												EunionHits (Eproj(1, Evar x), tr e (Etuple [Evar const, Eproj(0, Evar x)]))), 
											EallocEmptyHits, 
											Eproj (1, Evar gs)))
								| (HitCase, _) => raise NotYetImplemented
								| (FragCase, _) => raise NotYetImplemented
						in
							Elet (gs, carry, loop)
						end
				| (_, S.EsizeCase (dom, i, e1, e2)) => 
				
						let
							val gs = Variable.newvar "gs"
						in
							Elet (gs, carry, 
							Eif (
								case dom of
								  GeoCase => EsizeGt (Eproj(0, Evar gs), i)
								| SampCase => EsizeGt (Eproj(1, Evar gs), i)
								| _ => raise NotYetImplemented, 
								tr e1 (Evar gs),
								tr e2 (Evar gs)
							))
						end
						
				| (_, S.ErememberCase (e1, e2)) => raise NotYetImplemented
				| (_, S.Etest e) =>
						let
							val gs = Variable.newvar "gs"
						in
							Elet (gs, carry, 
							Eif (
								Eisect (Eproj(0, Eproj(0, Evar gs)), Eproj(0, Eproj(1, Evar gs))), 
								tr e (Evar gs),
								EallocBottomHits (Eproj (1, Eproj(1, Evar gs)))
							))
						end
				| (_, S.EfiltS e) => raise NotYetImplemented
				| (_, S.EbreakG (_, d)) => 
						let
							val gs = Variable.newvar "gs" 
						in
							Elet (gs, carry,
							Etuple [trD d (Eproj (0,Evar gs)), Eproj(1, Evar gs)])
						end
				| (_, S.EbreakS (_, d)) => 
						let
							val gs = Variable.newvar "gs" 
						in
							Elet (gs, carry,
							Etuple [Eproj (0,Evar gs), EbreakS (d, Eproj(1, Evar gs))])
						end
				| (_, S.EboundS (_)) => 
						let
							val gs = Variable.newvar "gs"
							val ray = Variable.newvar "ray"
						in
							Elet (gs, carry, 
							Elet (ray, Eproj (1, Evar gs),
							Etuple [Eproj (0, Evar gs), Etuple [EboundS (Evar ray), Evar ray]]))
						end
				| (_, S.EunboundG _) => 
						let
							val gs = Variable.newvar "gs" 
						in
							Elet (gs, carry,
							Etuple [Eproj(1, Eproj (0,Evar gs)), Eproj(1, Evar gs)])
						end
				| (_, S.EunboundS _) =>
						let
							val gs = Variable.newvar "gs" 
						in
							Elet (gs, carry,
							Etuple [ Eproj (0,Evar gs), Eproj(1, Eproj(1, Evar gs))])
						end
				| (_, S.Ehit) => 
						let
							val gs = Variable.newvar "gs"
						in
							Elet (gs, carry,
							Ehit (Eproj(0, Evar gs), Eproj(1, Evar gs)))
						end
				| (_, S.Eshade) => raise NotYetImplemented
                | (_, S.Efix (label, (inType, outType), e)) => 
					let
						val arg = Variable.newvar "arg"
					in
						addFunc (Func (trType outType, label, (trType inType, arg), tr e (Evar arg)));
						Ecall (label, carry)
					end
                | (_, S.Elabel label) => Ecall (label, carry)
                | (_, S.EunrollG (s,x,t)) => raise NotYetImplemented
			  
			and trD eSource carry = 
				case eSource of 
				  S.Dflat => carry
				| S.DsizeCase (i, e1, e2) => raise NotYetImplemented
				| S.Dbound e => 
					let
						val d = Variable.newvar "d"
						val bound = Variable.newvar "bound"
					in
						Elet (d, carry,
						Elet (bound, EboundG (Evar d),
						Etuple [Evar bound, trD e (Evar d)]))
					end
				| S.Dlayer (prim, S.Dflat) => EbreakG (prim, carry)
				| S.Dlayer (prim, e) => 
					let
						val elem = Variable.newvar "elem"
					in
						Emap ("layer_", 1, Elam (elem, Tint, trD e (Evar elem)), EbreakG (prim, carry))
					end
                | S.Dfix (label, outType, e) => 
					let
						val arg = Variable.newvar "arg"
					in
						addFunc (Func (trDomainType TgeomsFlat Bbox outType, label, (TgeomsFlat, arg), trD e (Evar arg)));
						Ecall (label, carry)
					end
                | S.Dvar label =>  Ecall (label, carry)
		
			val inputVar = Variable.newvar "arg"
			val (inType, outType) = wholeType
		in
			(Func (trType outType, Variable.newvar "root", (trType inType, inputVar), tr e (Evar inputVar)), !funcs : func list)
		end
end
