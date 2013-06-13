
signature SIZEBOUNDS =
sig

	datatype sizeBounds = SClosed of int * int
                        | SOpen of int
	
	type sizeTransition = sizeBounds * sizeBounds
	
	val boundsLT : sizeBounds -> sizeBounds -> bool
	val boundsJoin : sizeBounds -> sizeBounds -> sizeBounds
	val sizeTransitionLT : sizeTransition -> sizeTransition -> bool
end
