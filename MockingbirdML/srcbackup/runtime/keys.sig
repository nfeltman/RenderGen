
signature KeyBag =
sig
	type 'a keyed = 'a * int
	type 'a keyBag
	
	exception SizeMismatch
	
	val genBag : int * (int -> 'a) -> 'a keyBag
	val mergeBags : ('a -> 'a -> 'a) -> 'a keyBag -> 'a keyBag -> 'a keyBag
	val mapBag : ('a -> 'b ) -> 'a keyBag -> 'b keyBag
	val bagSize : 'a keyBag -> int
	val splitToIndividuals : 'a keyBag -> ('a keyBag) list
	val applyShortCircuit : ('a keyBag -> 'b keyBag) -> 'a keyBag -> 'b keyBag
	val filterBag : ('a -> bool) -> 'a keyBag -> ('a keyBag * 'a keyBag)
end
