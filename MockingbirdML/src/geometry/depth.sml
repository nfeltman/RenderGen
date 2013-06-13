
structure Depth :> DEPTH = 
struct
	datatype 'a depthed = Infty | Finite of real * 'a
	
	fun min (f1 as Finite (d1, _)) (f2 as Finite (d2, _)) = 
			if d1 < d2 then f1 else f2
	  | min d Infty = d
	  | min Infty d = d
      
    fun dmap _ Infty = Infty
      | dmap f (Finite (d,a)) = Finite (d, f a)
end

