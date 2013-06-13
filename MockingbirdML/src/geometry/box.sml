
structure Box3 = 
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

fun bound3 x y z = (Real.min(Real.min(x,y),z), Real.max(Real.max(x,y),z)) 
fun boundTri ((x1,y1,z1),(x2,y2,z2),(x3,y3,z3)) = (bound3 x1 x2 x3, bound3 y1 y2 y3, bound3 z1 z2 z3)

fun boundPoint (x,y,z) = ((x,x),(y,y),(z,z))

fun expand3 x x1 x2 = (Real.min (x,x1), Real.max(x,x2))
fun injectPoint (x,y,z) ((x1,x2),(y1,y2),(z1,z2)) = (expand3 x x1 x2, expand3 y y1 y2, expand3 z z1 z2)

fun centroid ((x1,x2),(y1,y2),(z1,z2)) = ((x1+x2)*0.5,(y1+y2)*0.5,(z1+z2)*0.5)

end 
