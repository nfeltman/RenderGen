
signature BOUNDS3 = 
sig

type neBound
datatype bound = Empty | NonEmpty of neBound 

structure NE_SL : SEMILATTICE where type t = neBound
structure SL : SEMILATTICE where type t = bound

val isect : neBound -> Vec3.ray3 -> Interval.ival

end 