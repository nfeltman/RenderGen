
structure MbArray :> MB_ARRAY = 
struct

	type 'a arr = int * 'a array
	type 'a cell = 'a array
	
	fun alloc arg = (0, Array.array arg)
	fun tabulate arg = (0, Array.tabulate arg)
	fun fromList el = (0, Array.fromList el)
	fun sub ((off,a),i) = Array.sub (a,off+i)
	fun setElem ((off,a),i,x) = Array.update (a,off+i,x)
	fun offset ((off,a),off2) = (off+off2,a)
	fun first (off,a) = Array.sub (a,off)
	fun fold a n f z = if n = 0 then z else f (first a, fold (offset (a,1)) (n-1) f z)
	fun app f a n = if n = 0 then () else (f (first a); app f (offset (a,1)) (n-1))
	
	fun swap (a,i1,i2) = 
		let
			val t = sub (a,i1)
		in
			setElem (a, i1, sub (a,i2));
			setElem (a, i2, t)
		end
	
	fun partition f a n = 
		let
			fun p a 0 n1 = n1
			  | p a n n1 =
				if f (first a) 
				then p (offset (a,1)) (n-1) (n1+1)
				else (swap (a,0,n-1); p a (n-1) n1)
		in
			p a n 0
		end

end
