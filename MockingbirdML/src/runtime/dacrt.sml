

structure DacrtRenderer = 
struct

exception InvariantException
open Runtime

fun render ((numTris, tris) : (Tri3.triangle) dynArray, (numrays, rays) : (Vec3.ray3) dynArray) : (Hit dynArray) = 
	let
		open MbArray
		val res = tabulate (numrays, fn _ => bottomHit)
		fun update k h = setElem (res, k, closer (h,(sub (res,k))))
		fun trace ((ng,g),box) (nr,r : (int * Vec3.ray3) arr) = 
			case nr of
			  0 => ()
			| _ => 
				let
					fun ivalTest i = case i of Interval.Empty => false | Interval.NonEmpty _ => true
					val filtered = (partition (ivalTest o (Box3.isect box) o #2) r nr, r)
				in
					case ng of
					  0 => raise InvariantException
					| 1 => let val t = first g in app (fn (k,r) => update k (isect (t,r))) r nr end
					| _ => case split (ng,g) of (t1, t2) => (trace t1 filtered; trace t2 filtered)
				end
		
		val raysInit = tabulate (numrays, fn i => (i, sub (rays,i)))
	in
		trace ((numTris,tris), findBounds numTris tris) (numrays, raysInit);
		(numrays, res)
	end

end
