
functor BaseInterpreter (K : KeyBag) = 
struct
	open K
	open Depth
    open BasicSyntax
	structure S = FixedPointSyntax (MbSyntax)
	datatype 'f Entity	= SmlGS of Tri3.triangle list * Vec3.ray3 keyBag
                        | SmlHit of ((Tri3.triangle*(real*real)) Depth.depthed) keyBag
                        | SmlFrag of ('f Depth.depthed) keyBag
	
	exception RuntimeTypeError	
	exception NotYetImplemented	
	
	fun assertSingle bag = if (bagSize bag) = 1 then () else raise RuntimeTypeError
	
	fun assertSingleList (one::[]) = one 
	  | assertSingleList _ = raise RuntimeTypeError
	
	fun assertNonEmpty (h::r) = (h, r)
	  | assertNonEmpty _ = raise RuntimeTypeError
	  
	fun assertGS (SmlGS gs) = gs
	  | assertGS (SmlHit _) = raise RuntimeTypeError
	  | assertGS (SmlFrag _) = raise RuntimeTypeError
    
	fun assertHits (SmlGS _) = raise RuntimeTypeError
	  | assertHits (SmlHit h) = h
	  | assertHits (SmlFrag _) = raise RuntimeTypeError
	
    fun doGeomSplit OneG geoms = List.map (fn g=>[g]) geoms
      | doGeomSplit TwoGP geoms = raise NotYetImplemented
    
    fun doSampSplit OneS samps = splitToIndividuals samps
      | doSampSplit SixteenSqSP samps = raise NotYetImplemented
    
    fun interpret shader = 
        let
            fun build subBuild (MbSyntax.EChain (e1, e2)) = 
                    let
                        val f1 = subBuild e1
                        val f2 = subBuild e2
                    in
                        fn c => fn data => f2 c (f1 c data)
                    end
              | build subBuild (MbSyntax.EMerge (m, d, e)) = 
                    let
                        val f = subBuild e
                        fun checkedMerge (SmlHit b1) (SmlHit b2) = SmlHit (mergeBags Depth.min b1 b2)
                          | checkedMerge (SmlFrag b1) (SmlFrag b2) = SmlFrag (mergeBags Depth.min b1 b2)
                          | checkedMerge _ _ = raise RuntimeTypeError
                    in
                        case (m,d) of 
                          (Union,PrimS(decomp)) => (
                            fn c => fn data => 
                            let 
                                val (geoms,samps) = assertGS data
                                val splitSamps = doSampSplit decomp samps
                                val gsTuples = List.map (SmlGS o (Util.leftSection geoms)) splitSamps
                            in
                                 Util.mapReduce (f c) checkedMerge (assertNonEmpty gsTuples)
                            end)
                        | (General,PrimG(decomp)) => (
                            fn c => fn data => 
                            let 
                                val (geoms,samps) = assertGS data
                                val splitGeoms = doGeomSplit decomp geoms
                                val gsTuples = List.map (SmlGS o (Util.rightSection samps)) splitGeoms
                            in
                                Util.mapReduce (f c) checkedMerge (assertNonEmpty gsTuples)
                            end)
                        | _ => raise RuntimeTypeError
                    end
              | build _ (MbSyntax.ESizeCase (dom, n, e1, e2)) = raise RuntimeTypeError
              | build _ (MbSyntax.EFilt (t, e)) = raise RuntimeTypeError
              | build _ MbSyntax.EHit = (
                    fn _ => fn data =>
                            let
                                val (g,s) = assertGS data
                                val _ = assertSingle s
                                val g1 = assertSingleList g
                                val addG1 = dmap (fn uv => (g1,uv))
                            in
                                SmlHit (mapBag (addG1 o (Tri3.intersectRay g1)) s)
                            end )
              | build _ MbSyntax.EShade = 
                    fn _ => fn data => 
                            let
                                val hits = assertHits data
                                val _ = assertSingle hits
                            in
                                SmlFrag (mapBag (dmap shader) hits)
                            end
        in
            S.interpret build
        end
end


functor LiftInterpreter (K : KeyBag) = 
struct
	open K
	open Depth
    open BasicSyntax
	structure S = FixedPointSyntax (MbSyntax)
    datatype 'a Register = Single of 'a | Many of 'a list
	datatype 'f Entity	= SmlGS of (Tri3.triangle list) Register * (Vec3.ray3 keyBag) Register
                        | SmlHit of (((Tri3.triangle*(real*real)) Depth.depthed) keyBag) Register
                        | SmlFrag of (('f Depth.depthed) keyBag) Register
	
	
end

