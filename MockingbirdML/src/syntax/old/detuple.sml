

structure Detuple = 
struct
    open List
    open MbDetupled
	structure P = MbPointed

	exception NotImplemented
    exception InconsistentType of (P.value * (value * value)) list * P.value
    
    fun trVal (P.Vvar v) = Vvar v
      | trVal (P.Vint i) = Vint i
	  
    val find = Util.find
	fun forcefind G v = case find G v of NONE => raise (InconsistentType (G,v)) | SOME gs => gs
	
    
	fun trDomainType P.Dflat = Dflat
	  | trDomainType (P.Darray d) = Darray (trDomainType d)
	  | trDomainType (P.Dbounded d) = raise NotImplemented (*Dbounded (trDomainType d)*)
	
	fun trType (P.Tgs (g,s)) = [Tgeom (trDomainType g), Tsamp (trDomainType s)] 
	  | trType P.Tint = [Tint]
	  | trType (P.Thit d) = [Thit (trDomainType d)]
    fun trTypeList ts = concat (map trType ts)
	
	fun expandVal G value = case find G value of NONE => [trVal value] | SOME (g,s) => [g,s]
    fun trValList G vs = concat (map (expandVal G) vs)
	
	fun trArg ((P.Tgs (gIn, sIn), arg),(G,newArgs)) = 
		let
			val g = Variable.newvar "g"
			val s = Variable.newvar "s"
		in
			((P.Vvar arg,(Vvar g,Vvar s))::G, (Tgeom (trDomainType gIn), g)::(Tsamp (trDomainType sIn), s)::newArgs )
		end
	  | trArg ((P.Tint, arg),(G,newArgs)) = (G,(Tint, arg)::newArgs)
	  | trArg ((P.Thit h, arg),(G,newArgs)) = (G,(Thit (trDomainType h), arg)::newArgs)
	
	fun trArgList G (al : (MbPointed.valType * variable) list) = foldr trArg (G,[]) al
	(*
	fun trOutVar (var,(G,newVars)) = 
		let
			val g = Variable.newvar "g"
			val s = Variable.newvar "s"
		in
			((Vvar var,(g,s))::G, g::s::newVars)
		end
	  | trOutVar (var,(G,newVars)) = (G, var::newVars)
	  | trOutVar (var,(G,newVars)) = (G, var::newVars)
	*)
	fun trRegion G (P.Rdef (outTypes, inArgs, exp)) = 
		let 
			val (newG, newArgs) = trArgList G inArgs
		in
			Rdef (trTypeList outTypes, newArgs, trExpr newG exp)
		end
	
	and trExpr G expr = 
			let
				val tr = trExpr G
			in
			case expr of
			  P.EforRange (sinks,s,e,body,init,rest) => (*  ERRROR! I need to map this sinks list, if it has GS types. *)
					EforRange (sinks, trVal s, trVal e, trRegion G body, trValList G init, trExpr G rest)
			| P.EallocBottomHits s => EallocBottomHits (tr s)
			| P.EallocEmptyHits => EallocEmptyHits
			| P.EmultiplexGeom (sinkGS, sinkType, sourceGS, i, rest) =>
					let
						val (sourceG, sourceS) = forcefind G sourceGS
						val g = Variable.newvar "g"
					in
						EgetElement (g, Tgeom (trDomainType sinkType), sourceG, trVal i, trExpr ((P.Vvar sinkGS,(Vvar g, sourceS))::G) rest)
					end
			| P.EmultiplexSamp (sinkGS, sinkType, sourceGS, i, rest) =>
					let
						val (sourceG, sourceS) = forcefind G sourceGS
						val s = Variable.newvar "s"
					in
						EgetElement (s, Tsamp (trDomainType sinkType), sourceS, trVal i, trExpr ((P.Vvar sinkGS,(sourceG, Vvar s))::G) rest)
					end
			| P.EgetGeomSize (sink, sourceGS, rest) => 
					let
						val (sourceG, _) = forcefind G sourceGS
					in
						EgetSize (sink, sourceG, trExpr G rest)
					end
			| P.EgetSampSize (sink, sourceGS, rest) => 
					let
						val (_, sourceS) = forcefind G sourceGS
					in
						EgetSize (sink, sourceS, trExpr G rest)
					end
			| P.Ecall (sink, outType, label, arg, rest) => (
					case outType of
					  P.Tgs _ => 
						let 
							val s = Variable.newvar "s"
							val g = Variable.newvar "g"
						in
							Ecall ([g,s], label, expandVal G arg, trExpr ((P.Vvar sink, (Vvar g, Vvar s))::G) rest )
						end
					| _ => Ecall ([sink], label, expandVal G arg, trExpr G rest) )
			| P.EboundG (sinkGS, sourceGS, rest) => raise NotImplemented
			| P.EbreakG (sinkGS, dec, sourceGS, rest) =>
					let
						val (sourceG, sourceS) = forcefind G sourceGS
						val g = Variable.newvar "g"
					in
						EbreakG (g, dec, sourceG, trExpr ((P.Vvar sinkGS,(Vvar g,sourceS))::G) rest)
					end
			| P.EbreakS (sinkGS, dec, sourceGS, rest) =>
					let
						val (sourceG, sourceS) = forcefind G sourceGS
						val s = Variable.newvar "s"
					in
						EbreakS (s, dec, sourceS, trExpr ((P.Vvar sinkGS,(sourceG,Vvar s))::G) rest)
					end
			| P.EcloserHits (h1, h2) => EcloserHits (tr h1, tr h2)
			| P.EunionHits (h1, h2) => EunionHits (tr h1, tr h2)
			| P.Ehit (sink, sourceGS, rest) => 
					let
						val (sourceG, sourceS) = forcefind G sourceGS
					in
						Ehit (sink, sourceG, sourceS, trExpr G rest)
					end
			| P.Eset (sink, source, rest) => 
					case find G source of 
					  NONE => Eset (sink, trVal source, trExpr G rest)
					| SOME gs => trExpr ((P.Vvar sink,gs)::G) rest
			
			end
	
	fun trFunction (P.Func (outType, name, inArg, expr)) =
			let 
				val (G, newArgs) = trArg (inArg, ([],[]))
			in
				Func (trType outType, name, newArgs, trExpr G expr)
			end
		
	fun translate (main, rest) = (trFunction main, map trFunction rest)
end

