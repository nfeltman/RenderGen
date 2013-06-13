
structure Vec3 :> VEC3 = 
struct

type vec3 = real * real * real
type seg3 = { originS: vec3, difference: vec3 }
type ray3 = { originR: vec3, direction: vec3 }
type shadowQuery = seg3 * bool 
val origin = (0.0, 0.0, 0.0)
fun length2 (x:real,y,z) = x * x + y * y + z * z
fun dot (ax:real,ay,az) (bx,by,bz) = ax * bx + ay * by + az * bz
fun mul (x,y,z) (s : real) = (s * x, s * y, s * z )
fun add (ax,ay,az) (bx,by,bz) : vec3 = (ax + bx, ay + by, az + bz)
fun sub (ax,ay,az) (bx,by,bz) : vec3 = (ax - bx, ay - by, az - bz)
fun cross (ax,ay,az) (bx,by,bz) : vec3 = 
	(ay * bz - az * by,
	 az * bx - ax * bz,
	 ax * by - ay * bx)
fun normalize v = mul v (1.0 / Math.sqrt (length2 v))
fun firstIsCloser f s v = length2 (sub f v) <= length2 (sub s v)
fun toString (x,y,z) = "(" ^ (Real.toString x) ^ "," ^ (Real.toString y) ^ "," ^ (Real.toString z) ^ ")"

(*
let readVector input = 
	let x = Util.read_single input in
	let y = Util.read_single input in
	let z = Util.read_single input in
	{x = x; y = y; z = z}
*)
	
end 