
signature TRI3 =
sig

type triangle = Vec3.vec3 * Vec3.vec3 * Vec3.vec3
val normal : triangle -> Vec3.vec3
val intersectsSegment : triangle -> Vec3.seg3 -> bool
val intersectRay : triangle -> Vec3.ray3 -> (real * real) Depth.depthed
val intersectsAny : triangle list -> Vec3.seg3 -> bool

val centroid : triangle -> Vec3.vec3

end 