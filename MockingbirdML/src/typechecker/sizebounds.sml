
structure SizeBounds =
struct

	datatype t 	= SClosed of int * int
				| SOpen of int
	
	fun lt (SClosed (n1,m1)) (SClosed (n2,m2)) = n1 >= n2 andalso m1 <= m2
	  | lt (SClosed (n1,_)) (SOpen n2) = n1 >= n2
	  | lt (SOpen _) (SClosed _) = false
	  | lt (SOpen n1) (SOpen n2) = n1 >= n2

	fun join (SClosed (n1,m1)) (SClosed (n2,m2)) = SClosed(Int.min (n1,n2), Int.max (m1,m2))
	  | join (SClosed (n1,_)) (SOpen n2) = SOpen(Int.min (n1,n2))
	  | join (SOpen n1) (SClosed (n2, _)) = SOpen(Int.min (n1,n2))
	  | join (SOpen n1) (SOpen n2) = SOpen(Int.min (n1,n2))
	  
	fun meet _ _ = raise Fail "undefined"
end


	val positiveBound = SOpen 1
	val justOne = SClosed (1,1)
	val unchanged = (positiveBound, positiveBound)
	val toOne = (positiveBound, justOne)
	
	fun getBasicDecompType OneG = GeoSamps (toOne, unchanged)
	  | getBasicDecompType TwoGP = GeoSamps ((SOpen 2, positiveBound), unchanged)
	  | getBasicDecompType OneS = GeoSamps (unchanged, toOne)
	  | getBasicDecompType SixteenSqSP = GeoSamps (unchanged, (positiveBound, positiveBound))
	  | getBasicDecompType OneH = Hits toOne
