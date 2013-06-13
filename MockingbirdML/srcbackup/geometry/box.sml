
structure Box3 :> BOUNDS3 = 
struct
structure I = Interval

type neBound = I.ne_ival * I.ne_ival * I.ne_ival
datatype bound = Empty | NonEmpty of neBound 

structure NE_SL = LatticeOps.CubedSL (I.NE_SL)

structure SL : SEMILATTICE = 
struct
    type t = bound
    
    fun lt Empty _ = true
      | lt (NonEmpty _) Empty = false
      | lt (NonEmpty b1) (NonEmpty b2) = NE_SL.lt b1 b2
      
    fun join Empty b = b
      | join b Empty = b
      | join (NonEmpty b1) (NonEmpty b2) = NonEmpty (NE_SL.join b1 b2)
end

fun isect (bx,by,bz) {originR = (ox,oy,oz), direction = (dx,dy,dz)} = 
		let
			fun range i ori dir = I.ne_div (I.ne_sub i ori) dir
		in
			I.L.meet (range bx ox dx) (I.L.meet (range by oy dy) (range bz oz dz))
		end

end 