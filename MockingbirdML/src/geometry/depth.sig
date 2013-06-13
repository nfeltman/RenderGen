
signature DEPTH = 
sig
	datatype 'a depthed = Infty | Finite of real * 'a
	
	val min : 'a depthed -> 'a depthed -> 'a depthed
    val dmap : ('a -> 'b) -> 'a depthed -> 'b depthed
end

