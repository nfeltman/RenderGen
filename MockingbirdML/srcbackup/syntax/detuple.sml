

structure Detuple = 
struct
    open List
    open MbDetupled
	structure P = MbPointed

    exception InconsistentType of (P.value * (value * value)) list * P.value
    
    fun trVal (P.Vvar v) = Vvar v
      | trVal (P.Vint i) = Vint i
	  
    val find = Util.find
	fun forcefind G v = case find G v of NONE => raise (InconsistentType (G,v)) | SOME gs => gs
	fun convert G v = case find G v of NONE => [trVal v] | SOME (g,s) => [g,s]
    fun convertValList vs G = concat (map (convert G) vs)    
    
	fun trType (P.Tgs (g,s)) = [Tgeom g, Tsamp s] 
	  | trType P.Tint = [Tint]
	  | trType (P.Thit d) = [Thit d]
	
	fun trFunction (outType, name, (inType,inputVar), expr) =
		let
			fun convFrees ((P.Tgs (gIn, sIn), source)::xs) =
				let 
					val s = Variable.newvar "s"
					val g = Variable.newvar "g"
					val (frees,gamma) = convFrees xs
				in
					((Tgeom gIn, g)::(Tsamp sIn, s)::frees, (P.Vvar source, (Vvar g, Vvar s))::gamma)
				end
			  | convFrees ((P.Tint, source)::xs) = (fn (f,g) => ((Tint, source)::f,g)) (convFrees xs)
			  | convFrees ((P.Thit h, source)::xs) = (fn (f,g) => ((Thit h, source)::f,g)) (convFrees xs)
			  | convFrees [] = ([],[])
		
			fun tr expr G = 
				case expr of
				  P.EforRange (i,s,e,free,body,init,rest) =>
						let							
							val (frees, gamma) = convFrees free
						in
							EforRange (i, trVal s, trVal e, frees, tr body gamma, convertValList init G, tr rest G)
						end
				| P.Econtinue vs => Econtinue (convertValList vs G)
				| P.EallocBottomHits (x,s,closed, e) => EallocBottomHits (x, trVal s, #1 (convFrees closed), tr e G)
				| P.EallocEmptyHits (v,e) => EallocEmptyHits (v, tr e G)
				| P.EmultiplexGeom (sinkGS, sinkType, sourceGS, i, rest) =>
						let
							val (sourceG, sourceS) = forcefind G sourceGS
							val g = Variable.newvar "g"
						in
							EgetElement (g, Tgeom sinkType, sourceG, trVal i, tr rest ((P.Vvar sinkGS,(Vvar g, sourceS))::G))
						end
				| P.EmultiplexSamp (sinkGS, sinkType, sourceGS, i, rest) =>
						let
							val (sourceG, sourceS) = forcefind G sourceGS
							val s = Variable.newvar "s"
						in
							EgetElement (s, Tsamp sinkType, sourceS, trVal i, tr rest ((P.Vvar sinkGS,(sourceG, Vvar s))::G))
						end
				| P.EgetGeomSize (sink, sourceGS, rest) => 
						let
							val (sourceG, _) = forcefind G sourceGS
						in
							EgetSize (sink, sourceG, tr rest G)
						end
				| P.EgetSampSize (sink, sourceGS, rest) => 
						let
							val (_, sourceS) = forcefind G sourceGS
						in
							EgetSize (sink, sourceS, tr rest G)
						end
				| P.Ecall (sink, outType, label, arg, rest) => (
						case outType of
						  P.Tgs _ => 
							let 
								val s = Variable.newvar "s"
								val g = Variable.newvar "g"
							in
								Ecall ([g,s], label, convert G arg, tr rest ((P.Vvar sink, (Vvar g, Vvar s))::G) )
							end
						| _ => Ecall ([sink], label, convert G arg, tr rest G) )
				| P.EbreakG (sinkGS, dec, sourceGS, rest) =>
						let
							val (sourceG, sourceS) = forcefind G sourceGS
							val g = Variable.newvar "g"
						in
							EbreakG (g, dec, sourceG, tr rest ((P.Vvar sinkGS,(Vvar g,sourceS))::G))
						end
				| P.EbreakS (sinkGS, dec, sourceGS, rest) =>
						let
							val (sourceG, sourceS) = forcefind G sourceGS
							val s = Variable.newvar "s"
						in
							EbreakS (s, dec, sourceS, tr rest ((P.Vvar sinkGS,(sourceG,Vvar s))::G))
						end
				| P.EcloserHits (sink, h1, h2, closed, rest) => EcloserHits (sink, trVal h1, trVal h2, #1 (convFrees closed), tr rest G)
				| P.EunionHits (sink, h1, h2, rest) => EunionHits (sink, trVal h1, trVal h2, tr rest G)
				| P.Ehit (sink, sourceGS, rest) => 
						let
							val (sourceG, sourceS) = forcefind G sourceGS
						in
							Ehit (sink, sourceG, sourceS, tr rest G)
						end
				| P.Eret v => Eret (convert G v)
				| P.Eset (sink, source, rest) => 
						case find G source of 
						  NONE => Eset (sink, trVal source, tr rest G)
						| SOME gs => tr rest ((P.Vvar sink,gs)::G)
		in
			case inType of
			  (P.Tgs (gt,st)) =>
				let 
					val s = Variable.newvar "s"
					val g = Variable.newvar "g"
				in
					(trType outType, name, [(Tgeom gt,g),(Tsamp st, s)], tr expr [(P.Vvar inputVar, (Vvar g, Vvar s))])
				end
			| P.Tint => (trType outType, name, [(Tint, inputVar)], tr expr []) : func
			| P.Thit d => (trType outType, name, [(Thit d, inputVar)], tr expr []) : func
		end
		
	fun translate program = map trFunction program
end

