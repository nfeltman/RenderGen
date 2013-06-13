

structure Deblock = 
struct
    open List
    open MbDeblocked
	structure P = MbDetupled
	structure C = CommonSyntax.OrderTypes

    exception InconsistentType
    
	fun trDomainType base C.Tflat = base
	  | trDomainType base (C.Tarray d) = Tarray [trDomainType base d]
	  | trDomainType base (C.Tsum (d1,d2)) = raise InconsistentType
	  | trDomainType base (C.Tbounded d) = raise InconsistentType
	  | trDomainType base (C.Tfix (l,d)) = raise InconsistentType
	  | trDomainType base (C.Tvar l) = raise InconsistentType
	
	fun trType (P.Tgeom d) = trDomainType (Tarray [Tgeom]) d
	  | trType (P.Tsamp d) = trDomainType (Tarray [Tint, Tsamp]) d
	  | trType P.Tint = Tint
	  | trType (P.Thit d) = trDomainType (Tarray [Tint, Thit]) d
	
    fun trVal (P.Vvar v) = Vvar v
      | trVal (P.Vint i) = Vint i
	  
    val find = Util.find
	fun forcefind G v = case find G v of NONE => raise InconsistentType | SOME gs => gs
	fun convert G v = case find G v of NONE => [trVal v] | SOME (g,s) => [g,s]
    fun convertValList vs G = concat (map (convert G) vs)
    
	fun trFunction (outTypes, name, args, expr) =
		let
		fun convArg (ty,name) = (trType ty,name)
		fun tr expr = 
			case expr of
			  P.EforRange (i,s,e,free,body,init,rest) => 
				let
					val convertedFree = map convArg free
				in
					EforRange (i, trVal s, trVal e, convertedFree, tr body, map trVal init, tr rest)
				end
			| P.Econtinue vs => Econtinue (map trVal vs)
			| P.EallocBottomHits (x,s,closed,e) => 
					let
						val i = Variable.newvar "iter"
						val args = (Tarray [Tint, Thit], x) :: (map convArg closed)
						val argVals = map (Vvar o #2 ) args
					in
						EallocHitsArray (x, trVal s, 
						EforRange (i, Vint 0, trVal s, args, 
							EsetElement (Vvar x, Vvar i, 0, Vvar i, [Tint, Thit],
							EsetElement (Vvar x, Vvar i, 1, VbotHit, [Tint, Thit],
							Econtinue argVals)),
						argVals, 
						tr e))
					end
			| P.EgetSize (sink, arr, rest) => EgetSize (sink, trVal arr, tr rest)
			| P.EallocEmptyHits (v,e) => EallocHitsArray (v, Vint 0, tr e)
			| P.EgetElement (sink, ty, arr, i, rest) => EgetElement (sink, [trType ty], trVal arr, trVal i, 0, tr rest)
			| P.Ecall (sinks, label, args, rest) => Ecall (sinks, label, map trVal args, tr rest)
			| P.EbreakG (sink, dec, source, rest) => EbreakG (sink, dec, trVal source, tr rest)
			| P.EbreakS (sink, dec, source, rest) => EbreakS (sink, dec, trVal source, tr rest)
			| P.EcloserHits (sink, h1, h2, closed, rest) => 
					let
						val size = Variable.newvar "size"
						val i = Variable.newvar "iter"
						val elem1 = Variable.newvar "h1"
						val elem2 = Variable.newvar "h2"
						val arr1 = Variable.newvar "arr1"
						val arr2 = Variable.newvar "arr2"
						val merged = Variable.newvar "merged"
						val convertedClosed = map convArg closed
						val closedVals = map (Vvar o #2) convertedClosed
					in
						EgetSize (size, trVal h1, 
						EforRange (i, Vint 0, Vvar size, (Tarray [Tint, Thit], arr1) :: (Tarray [Tint, Thit], arr2) :: convertedClosed, 
							EgetElement (elem1, [Tint, Thit], Vvar arr1, Vvar i, 1, 
							EgetElement (elem2, [Tint, Thit], Vvar arr2, Vvar i, 1, 
							EcloserHit (merged, Vvar elem1, Vvar elem2,
							EsetElement (Vvar arr1, Vvar i, 1, Vvar merged, [Tint, Thit],
							Econtinue (Vvar arr1 :: Vvar arr2 :: closedVals ))))),
						trVal h1 :: trVal h2 :: closedVals,
						Eset (sink, Vvar arr1, tr rest)))
					end
			| P.EunionHits (sink, h1, h2, rest) => EunionHits (sink, trVal h1, trVal h2, tr rest)
			| P.Ehit (sink, g, s, rest) => 
					let
						val g1 = Variable.newvar "g1"
						val s1 = Variable.newvar "s1"
						val k1 = Variable.newvar "k1"
						val h1 = Variable.newvar "h1"
					in
						EgetElement(g1, [Tgeom], Vvar g1, Vint 0, 0,
						EgetElement(k1, [Tint, Thit], Vvar g1, Vint 0, 0,
						EgetElement(s1, [Tint, Thit], Vvar g1, Vint 0, 1,
						EallocHitsArray (sink, Vint 1,
						EsetElement(Vvar sink, Vint 0, 0, Vvar k1, [Tint, Thit],
						EsetElement(Vvar sink, Vint 0, 1, Vvar s1, [Tint, Thit],
						tr rest))))))
					end
			| P.Eret v => Eret (map trVal v)
			| P.Eset (sink, source, rest) => Eset (sink, trVal source, tr rest)
		in
			(map trType outTypes, name, map convArg args, tr expr) : func
		end

	fun translate program = map trFunction program
end

