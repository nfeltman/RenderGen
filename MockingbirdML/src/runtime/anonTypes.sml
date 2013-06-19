

structure AnonTypes = 
struct

datatype ('a,'b) anonSum = AsLeft of 'a | AsRight of 'b

functor Recur (A : sig type 'a r end) =
struct
	datatype t = T of t A.r
	fun unroll (T z : t) = z : (t A.r)
	fun roll (z : t A.r) = (T z) : t
end
end
