
structure Interval = 
struct

type ne_ival = real * real
datatype ival = Empty | NonEmpty of ne_ival

val ne_all = (Real.negInf, Real.posInf)
val i_all = NonEmpty ne_all

fun ne_contains (l,u) (s :real) = l <= s andalso u >= s
fun i_contains Empty _ = false
  | i_contains (NonEmpty i) s = ne_contains i s

fun ne_add (l,u) (s :real) = (l+s,u+s)
fun i_add Empty _ = Empty
  | i_add (NonEmpty i) s = NonEmpty (ne_add i s)

fun ne_sub (l,u) (s :real) = (l-s,u-s)
fun i_sub Empty _ = Empty
  | i_sub (NonEmpty i) s = NonEmpty (ne_sub i s)

fun ne_mul (l,u) (s :real) = if s > 0.0 then (l*s,u*s) else (u*s,l*s)
fun i_mul Empty _ = Empty
  | i_mul (NonEmpty i) s = NonEmpty (ne_mul i s)

fun ne_div i s = 
		case Real.compareReal (s,0.0) of
		  IEEEReal.EQUAL => if ne_contains i 0.0 then i_all else Empty
		| _ => NonEmpty (ne_mul i (1.0/s)) 
fun i_div Empty _ = Empty
  | i_div (NonEmpty i) s = ne_div i s

structure NE_SL : SEMILATTICE =
struct
type t = ne_ival

fun lt (l1 : real, u1 : real) (l2,u2) = l1 >= l2 andalso u1 <= u2
fun join (l1,u1) (l2,u2) = (Real.min (l1,l2), Real.max (u1,u2))
end

structure L : LATTICE =
struct
type t = ival

fun lt Empty _ = true
  | lt (NonEmpty _) Empty = false
  | lt (NonEmpty i1) (NonEmpty i2) = NE_SL.lt i1 i2
  
fun join Empty i = i
  | join i Empty = i
  | join (NonEmpty i1) (NonEmpty i2) = NonEmpty (NE_SL.join i1 i2)
  
fun meet Empty _ = Empty
  | meet _ Empty = Empty
  | meet (NonEmpty (l1,u1)) (NonEmpty (l2,u2)) = 
		let
			val l = Real.max (l1,l2)
			val u = Real.min (u1,u2)
		in
			if l > u then Empty else NonEmpty (l,u)
		end
end



end 