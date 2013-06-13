structure Tri3 :> TRI3 = 
struct

open Vec3

type triangle = vec3 * vec3 * vec3

val t_EPSILON = 0.0001;;

fun normal (a,b,c) = (normalize (cross (sub c a) (sub b a)))

fun intersectsSegment triangle {originS = ori, difference = diff} = 
	let
		val (p1, p2, p3) = triangle 
		val (edge0, edge1, edge2) = (sub p1 p3, sub p3 p2, sub p3 ori)
		val (normal) = (cross edge0 edge1)
		val rcp = 1.0 / (dot normal diff) 
		val t = (dot normal edge2) * rcp
	in
		(t >= t_EPSILON andalso t <= (1.0 - t_EPSILON)) andalso
		let 
			val interm = cross edge2 diff
			val u = (dot interm edge1) * rcp
		in
			(u >= 0.0) andalso 
			let 
				val v = (dot interm edge0) * rcp 
			in
				(u + v) <= 1.0 andalso v >= 0.0
			end
		end
	end
	
fun intersectRay triangle {originR = ori, direction = dir} = 
	let
		val (p1, p2, p3) = triangle 
		val (edge0, edge1, edge2) = (sub p1 p3, sub p3 p2, sub p3 ori)
		val (normal) = (cross edge0 edge1)
		val rcp = 1.0 / (dot normal dir) 
		val t = (dot normal edge2) * rcp
		val interm = cross edge2 dir
		val u = (dot interm edge1) * rcp
		val v = (dot interm edge0) * rcp 
	in
		if t >= 0.0 andalso u >= 0.0 andalso v >= 0.0 andalso (u + v) <= 1.0 
		then Depth.Finite (t,(u,v))
		else Depth.Infty
	end

fun intersectsAny tris seg = List.exists (fn tri => intersectsSegment tri seg) tris

fun centroid (v1,v2,v3) = mul (add v1 (add v2 v3)) (1.0/3.0)

end