

signature MB_ARRAY = 
sig

	type 'a arr
	type 'a cell
	
	val alloc : int * 'a -> 'a arr
	val sub : 'a arr * int -> 'a
	val setElem : 'a arr * int * 'a -> unit
	val swap : 'a arr * int * int -> unit
	val offset : 'a arr * int -> 'a arr
	val first : 'a arr -> 'a
	val tabulate : int * (int -> 'a) -> 'a arr
	val fromList : 'a list -> 'a arr
	val fold : 'a arr -> int -> ('a * 'b -> 'b) -> 'b -> 'b

end