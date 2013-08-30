

structure BvhRenderer = 
struct

exception InvariantException
open Runtime

datatype openNode	= BvhBranch of closedNode * closedNode
					| BvhLeaf of Tri3.triangle
withtype closedNode	= Box3.neBound * openNode
(*
fun render ((numTris, tris) : (Tri3.triangle) dynArray, (numrays, rays) : Vec3.ray3 dynArray) : (Hit dynArray) = 
	let
		fun build ((n,g),box) = 
			case n of
			  0 => raise InvariantException
			| 1 => (box, BvhLeaf (MbArray.sub (g,0)))
			| _ => case split (n,g) of (t1, t2) => (box,BvhBranch (build t1, build t2))
		
		fun traverseOpen (b,t) r = 
				case Box3.isect b r of 
				  Interval.Empty => bottomHit
				| Interval.NonEmpty _ => traverseClosed t r
		
		and traverseClosed (BvhLeaf t) r = isect (t,r)
		  | traverseClosed (BvhBranch (c1,c2)) r = closer (traverseOpen c1 r, traverseOpen c2 r)
		
		val _ = print "building\n"
		val bvh = build ((numTris,tris), findBounds numTris tris)
		
		fun s (_,BvhBranch (c1,c2)) = s c1 + s c2
		  | s (_,BvhLeaf _) = 1
		  
		val _ = print "size = "
		val _ = print (Int.toString (s bvh))
		val _ = print (Int.toString numTris)
		val _ = print ";  done building \n"
		  
	in
		(numrays, MbArray.tabulate (numrays, fn i => traverseOpen bvh (MbArray.sub (rays,i))))
	end
*)
end
