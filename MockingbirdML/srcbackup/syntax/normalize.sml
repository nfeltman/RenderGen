
structure Normalize = 
struct
	
	open MbNormal
	structure S = MbOrder 
    
	exception TypeMismatch
    exception NotYetImplemented
	
	fun translate e wholeType =
		let
			val funcs = ref []
			fun addFunc z = (funcs := z :: !funcs)
			
			fun tr ty eSource rest =
				case (ty,eSource) of 
				  ((tIn,tOut), S.Echain (tMid, e1, e2)) => tr (tIn, tMid) e1 (tr (tMid, tOut) e2 rest)
				| ((GeoSamps (Tarray gIn, sIn),tOut), S.Emmr (GeoCase, e)) => Emmr (GeoCase, tr (GeoSamps (gIn, sIn),tOut) e Eemit, tOut, rest)
				| ((GeoSamps (gIn, Tarray sIn),tOut), S.Emmr (SampCase, e)) => Emmr (SampCase, tr (GeoSamps (gIn, sIn),tOut) e Eemit, tOut, rest)
				| (ty,S.EsizeCase (dom, i, e1, e2)) => EsizeCase (dom, i, tr ty e1 Eemit, tr ty e2 Eemit, rest)
				| ((GeoSamps (Tsum (gIn1, gIn2),sIn), tOut),S.ErememberCase (e1, e2)) => ErememberCase (tr (GeoSamps (gIn1,sIn), tOut) e1 Eemit, tr (GeoSamps (gIn2,sIn), tOut) e2 Eemit, rest)
				| (ty,S.Etest e) => Etest (tr ty e rest)
				| (_,S.EboundG (_, Single)) => EboundG rest
				| (_,S.EunboundG (_, Single)) => EunboundG rest
				| (_,S.EboundS (_, Single)) => EboundG rest
				| (_,S.EunboundS (_, Single)) => EunboundG rest
				| (ty,S.EfiltS e) => EfiltS (tr ty e rest)
				| (_,S.EbreakG (s, d)) => trD d s rest []
				| (_,S.EbreakS (_, d)) => EbreakS (d, rest)
				| (_,S.Ehit) => Ehit rest
				| (_,S.Eshade) => Eshade rest
                | (t as (_,tOut),S.Efix (label, eType, e)) => (addFunc (label, eType, tr t e Eemit); Ecall (label, tOut, rest))
                | ((_,tOut),S.Elabel label) => Ecall (label, tOut, rest)
                | (_,S.EunrollG (s,x,t)) => EunrollG (s,x,t,rest)
				| _ => raise TypeMismatch
			  
			and trD eSource sType rest G = 
				case eSource of 
				  S.Dflat => rest
				| S.DsizeCase (i, e1, e2) => EsizeCase (GeoCase, i, trD e1 sType (Einj (LEFT, Eemit)) G, trD e2 sType (Einj (RIGHT,Eemit)) G, rest)
				| S.Dbound e => EpreBoundGStruct (trD e sType Eemit G, rest)
				| S.Dlayer (prim, S.Dflat) => EbreakG (prim, rest) (* optimization! *)
				| S.Dlayer (prim, e) => EbreakG (prim, EmapBuildG (trD e sType Eemit G, rest))
                | S.Dfix (label, eType, e) => (addFunc (label, (GeoSamps (Tflat, sType), GeoSamps (eType, sType)), trD e sType Eemit ((label,eType)::G)); Ecall (label, GeoSamps (eType, sType), rest))
                | S.Dlabel label => case Util.find G label of NONE => raise TypeMismatch | SOME ret => Ecall (label, GeoSamps (ret, sType), rest)
		in
			(Variable.newvar "root", wholeType, tr wholeType e Eemit) :: !funcs
		end
end
