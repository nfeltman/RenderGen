
functor Renderers (B : BOUNDS3) (K : KeyBag) = 
struct
	open K
	structure I = Interval
	
    fun boundAll _ = raise Empty
    fun split _ = raise Empty
    fun calcBound _ = raise Empty
    
    datatype contents = Leaf of Tri3.triangle | Branch of boundedBVH*boundedBVH
	withtype boundedBVH = B.neBound * contents
	
	fun smart_bvh geometry rays = 
		let 
			fun buildBVH (g0,[]) = (calcBound g0, Leaf g0)
			  | buildBVH (g0,(g1::gs)) = 
					let
						val (half1,half2) = split (g0,g1,gs)
						val c1 as (bound1,_) = buildBVH half1
						val c2 as (bound2,_) = buildBVH half2
					in 
						(B.NE_SL.join bound1 bound2, Branch (c1,c2))
					end
			
			fun traverseClosed (bound,n) r = 
					case B.isect bound r of
					  I.Empty => Depth.Infty
					| I.NonEmpty _ => traverseOpen n r
			
			and traverseOpen (Leaf t) r = Tri3.intersectRay t r
			  | traverseOpen (Branch (c1,c2)) r = Depth.min (traverseClosed c1 r) (traverseClosed c2 r)
		in
			mapBag (traverseClosed (buildBVH geometry)) rays
		end
		
	fun redundant_decomp geometry rays = 
			let
				fun decompClosed geometry r = 
						case B.isect (boundAll geometry) r of
						  I.Empty => Depth.Infty
						| I.NonEmpty _ => decompOpen geometry r
						
				and decompOpen (g0,[]) r = Tri3.intersectRay g0 r
				  | decompOpen (g0,(g1::gs)) r = 
						let
							val (half1,half2) = split (g0,g1,gs)
						in 
							Depth.min (decompClosed half1 r) (decompClosed half2 r)
						end
			in
				mapBag (decompClosed geometry) rays
			end
			
	fun naive geometry rays = 
			let
				fun predicate b r = 
						case B.isect b r of
						  I.Empty => false
						| I.NonEmpty _ => true
				
				fun naiveClosed geometry rays = 
						let
							val (pass, fail) = filterBag (predicate (boundAll geometry)) rays
							val misses = mapBag (fn _ => Depth.Infty) fail 
							val maybeHits = applyShortCircuit (naiveOpen geometry) pass
						in
							mergeBags Depth.min misses maybeHits
						end
						
				and naiveOpen (g0,[]) rays = mapBag (Tri3.intersectRay g0) rays
				  | naiveOpen (g0,(g1::gs)) rays = 
						let
							val (half1,half2) = split (g0,g1,gs)
						in 
							mergeBags Depth.min (naiveClosed half1 rays) (naiveClosed half2 rays)
						end
			in
				naiveClosed geometry rays
			end
end



