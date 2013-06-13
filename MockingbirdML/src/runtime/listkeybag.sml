
structure ListKeyBag : KeyBag =
struct
	open List

	type 'a keyed = 'a * int
	type 'a keyBag = ('a keyed) list
	
	exception SizeMismatch
	
	fun genBag (n,f) = tabulate (n, fn i => (f i,i))
	
	fun mergeBags m (X as (x, ix)::xs) (Y as (y, iy)::ys) = (
			case Int.compare (ix,iy) of
			  LESS => (x,ix) :: (mergeBags m xs Y)
			| GREATER => (y,iy) :: (mergeBags m X ys)
			| EQUAL => (m x y,ix) :: (mergeBags m xs ys) )
	  | mergeBags _ [] y = y 
	  | mergeBags _ x [] = x 
	
	fun mapBag f = map (fn (a, i)=> (f a, i))
	
	val bagSize = length
	
	fun splitToIndividuals bag = map (fn x => [x]) bag
	
	
	fun applyShortCircuit _ = raise SizeMismatch
	fun filterBag _ = raise SizeMismatch
end
