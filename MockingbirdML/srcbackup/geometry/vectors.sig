
signature VEC3 = 
sig

type vec3 = real * real * real
type seg3 = { originS: vec3, difference: vec3 }
type shadowQuery = seg3 * bool 
type ray3 = { originR: vec3, direction: vec3 }
val origin : vec3
val length2 : vec3 -> real
val normalize : vec3 -> vec3
val dot : vec3 -> vec3 -> real
val mul : vec3 -> real -> vec3
val add : vec3 -> vec3 -> vec3
val sub : vec3 -> vec3 -> vec3
val cross : vec3 -> vec3 -> vec3
val firstIsCloser : vec3 -> vec3 -> vec3 -> bool


(* val readVector : in_channel -> vec3 *)

end