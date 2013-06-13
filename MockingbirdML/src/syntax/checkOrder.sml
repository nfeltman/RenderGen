

structure CheckOrder =
struct
	open CommonSyntax
	open MbOrder
	
	exception BadLabel
	exception BadType of string
	
	fun findLabel label ((l,t)::gs) = if label = l then t else findLabel label gs
	  | findLabel _ [] = raise BadLabel
	
	fun subst _ _ Tflat = Tflat
      | subst x t (Tarray d) = Tarray (subst x t d)
      | subst x t (Tsum (d1,d2)) = Tsum (subst x t d1, subst x t d2)
      | subst x t (Tbounded d) = Tbounded (subst x t d)
      | subst x t (f as Tfix (l,d)) = if l = x then f else Tfix (l, subst x t d)
      | subst x t (v as Tvar l) = if l = x then t else v
	
	fun inferType e = 
			let 
				fun assertSameD Tflat Tflat ex = ()
				  | assertSameD (Tarray d1) (Tarray d2) ex = assertSameD d1 d2 ex
				  | assertSameD (Tsum (c1,d1)) (Tsum (c2,d2)) ex = (assertSameD c1 c2 ex; assertSameD d1 d2 ex)
				  | assertSameD (Tbounded d1) (Tbounded d2) ex = assertSameD d1 d2 ex
				  | assertSameD (Tfix (x1,d1)) (Tfix (x2,d2)) ex = assertSameD d1 (subst x2 (Tvar x1) d2) ex
				  | assertSameD (Tvar x1) (Tvar x2) ex = if x1 = x2 then () else raise ex
				  | assertSameD _ _ ex = raise ex
						
				fun assertSameS (GeoSamps (t1,u1)) (GeoSamps (t2,u2)) ex = (assertSameD t1 t2 ex; assertSameD u1 u2 ex)
				  | assertSameS (Hits t1) (Hits t2) ex = assertSameD t1 t2 ex
				  | assertSameS (Frags t1) (Frags t2) ex = assertSameD t1 t2 ex
				  | assertSameS _ _ ex = raise ex
				  
				fun assertSameT (t1,u1) (t2,u2) ex = (assertSameS t1 t2 ex; assertSameS u1 u2 ex)
				
				fun sameD d1 d2 ex = (assertSameD d1 d2 ex; d1)
				fun sameS s1 s2 ex = (assertSameS s1 s2 ex; s1)
				fun sameT t1 t2 ex = (assertSameT t1 t2 ex; t1)
								
				datatype maybeType	= AllKnown of domainType stage * domainType stage
													
				fun chD expr g = 
					case expr of
					  Dflat => Tflat
                    | DsizeCase (i,d1,d2) => Tsum (chD d1 g, chD d2 g)
                    | Dbound d => Tbounded (chD d g)
                    | Dlayer (p, d) => Tarray (chD d g)
                    | Dfix (l,t,d) => 
						let 
							val newLabel = Variable.addSuffix l "_type"
						in 
							sameD (Tfix (newLabel, chD expr ((l,newLabel)::g))) t (BadType "decompFix")
						end
                    | Dlabel l => Tvar (findLabel l g)
				
				fun chE expr g =
					case expr of
					  Echain (e1, e2) => (
							case (chE e1 g, chE e2 g) of 
							  ((s1, d1 as GeoSamps _), (d2 as GeoSamps _, s2)) => (sameS d1 d2 (BadType "chain"); (s1,s2))
							| ((s1, Hits d1), (Hits d2, s2)) => (sameD d1 d2 (BadType "chain"); (s1,s2))
							| ((s1, Frags d1), (Frags d2, s2)) => (sameD d1 d2 (BadType "chain"); (s1,s2))
							| _ => raise (BadType "chain"))
                    | Emmr (dom, e) => (
							case (dom, chE e g) of 
							  (GeoCase, (GeoSamps (g,s), out)) => (GeoSamps (Tarray g, s), out)
							| (SampCase, (GeoSamps (g,s), out)) => (GeoSamps (g, Tarray s), out)
							| (HitCase, (Hits h, out)) => (Hits (Tarray h), out)
							| _ => raise (BadType "mmr"))
                    | EsizeCase (_, i, e1, e2) => (
							case (chE e1 g, chE e2 g) of 
							  (t1 as (GeoSamps _, _), t2 as (GeoSamps _, _)) => sameT t1 t2 (BadType "sizeCase")
							| _ => raise (BadType "sizeCase"))
                    | ErememberCase (e1, e2) => (
							case (chE e1 g, chE e2 g) of 
							  ((GeoSamps (t1,u1), v1), (GeoSamps (t2,u2), v2)) => 
									(GeoSamps (Tsum (t1,t2), sameD u1 u2 (BadType "rememberCase")), sameS v1 v2 (BadType "rememberCase"))
							| _ => raise (BadType "rememberCase"))
                    | Etest e => (
							case chE e g  of 
							  a as (GeoSamps (Tbounded _, Tbounded _), _) => a
							| _ => raise (BadType "test"))
                    | EfiltS e => (
							case chE e g of 
							  a as (GeoSamps (Tbounded _, Tarray (Tbounded _)), _) => a
							| _ => raise (BadType "filtS"))
                    | EbreakG (s,dec) => (GeoSamps (Tflat, s), GeoSamps(chD dec [], s))
                    | EbreakS (g,dec) => (GeoSamps (g, Tflat), GeoSamps(g, Tarray Tflat))
                    | Ehit => (GeoSamps (Tflat, Tflat), Hits Tflat)
                    | Eshade => (Hits Tflat, Frags Tflat)
                    | EunboundG (g,s) => (GeoSamps (Tbounded g,s), GeoSamps (g,s))
                    | EboundS g => (GeoSamps (g, Tflat), GeoSamps (g, Tbounded Tflat))
                    | EunboundS (g,s) => (GeoSamps (g, Tbounded s), GeoSamps (g, s))
                    | Efix (label, ty, e) => sameT (chE e ((label,ty)::g)) ty (BadType "fix")
                    | Elabel (label) => findLabel label g
					| EunrollG (s,x,t) => (GeoSamps (Tfix (x,t),s), GeoSamps (subst x (Tfix (x,t)) t, s))
			in
				chE e []
			end
    
    
end 