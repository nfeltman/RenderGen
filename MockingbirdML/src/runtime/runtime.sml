
structure Runtime = struct
	
	exception RuntimeTypeError
	exception NotYetImplemented	
	
	(*
	fun assertSingle bag = if (bagSize bag) = 1 then () else raise RuntimeTypeError
	
	
	type gsEntity = Tri3.triangle list * Vec3.ray3 keyBag
	type hitEntity = ((Tri3.triangle*(real*real)) Depth.depthed) keyBag
	type fragEntity = (real Depth.depthed) keyBag
	*)
	
	(* Arrays *)
	type 'a arr = ('a option) MbArray.arr
	type 'a dynArray = int * 'a arr
	fun sub x = valOf (MbArray.sub x)
	fun first x = valOf (MbArray.first x)
	val offset = MbArray.offset
	fun partition f = MbArray.partition (f o valOf)
	fun alloc (x, z) = MbArray.alloc (x, SOME z)
	fun tabulate (x, f) = MbArray.tabulate (x, SOME o f)
	fun fromList l = MbArray.fromList (map SOME l)	
	fun setElem (a,i,x) = MbArray.setElem (a,i, SOME x)
	fun fold a n f = MbArray.fold a n (fn (x,y) => f (valOf x, y))
	fun allocHits n = (n, MbArray.tabulate (n, fn _ => NONE))
	
	(* Bounds *)
	fun findBounds n t = 
		if n < 1 then raise RuntimeTypeError
		else if n = 1 then Box3.boundTri (first t) 
		else Box3.NE_SL.join (Box3.boundTri (first t)) (findBounds (n-1) (offset (t,1)))
	fun boundGeoms (n,t) = findBounds n t
	
	fun findCentroidBounds n t = 
		if n < 1 then raise RuntimeTypeError
		else if n = 1 then (Box3.boundPoint (Tri3.centroid (first t))) 
		else Box3.injectPoint (Tri3.centroid (first t)) (findCentroidBounds (n-1) (offset (t,1)))
	
	val eq = Real.==
	fun split (n,a) = 
			let
				val b as ((x1,x2),(y1,y2),(z1,z2)) = findCentroidBounds n a
				val lens as (xl,yl,zl) = (x2-x1, y2-y1, z2-z1)
				val mid as (xm,ym,zm) = Box3.centroid b
				val filt =
					if xl > yl andalso xl > zl then (fn (x,_,_) => x < xm)
					else if yl > zl then (fn (_,y,_) => y < ym)
					else (fn (_,_,z) => z < zm)
								
				val n1 = 
					if n < 2 then raise RuntimeTypeError 
					else if eq (xl,0.0) andalso eq (yl, 0.0) andalso eq (zl, 0.0) then Int.div (n,2)
					else partition (filt o Tri3.centroid) a n
					
				val a2 = offset (a,n1)
				val n2 = n - n1
				
				val _ = if n1 = 0 orelse n2 = 0 then raise RuntimeTypeError else ()
			in
				(((n1,a), findBounds n1 a),((n2,a2), findBounds n2 a2))
			end
	
	(* Primitive Ops *)
	type box = Box3.NE_SL.t
	fun isect (g,s) = Tri3.intersectRay g s
	fun isectBoxRay (b : box, r : Vec3.ray3) = 
			case Box3.isect b r of
			  Interval.NonEmpty _ => true
			| Interval.Empty => false
	
	(* Hits *)
	type Hit = (real * real) Depth.depthed
	val bottomHit : Hit = Depth.Infty
	fun closer (h1,h2) = Depth.min h1 h2
	fun unionHits ((size1,hArr1),(size2,hArr2)) : Hit dynArray = 
		let
			fun incr a = offset (a,1)
			fun setRest m 0 _ = m
			  | setRest m n a = (setElem (m, 0, first a); setRest (incr m) (n-1) (incr a)) 
			val merged = alloc (size1 + size2, bottomHit)
		in
			setRest (setRest merged size1 hArr1) size2 hArr2;
			(size1+size2, merged)
		end
	
	(* Decomps *)
	fun performOneS (size,samps) = (size, tabulate (size, fn i => (1, fromList ([sub (samps,i)]))))
	fun performOneG (size,geos) = (size, tabulate (size, fn i => (1, fromList ([sub (geos,i)]))))
	fun performTwoGP args = 
			let
				val ((a1,_),(a2,_)) = split args
			in
				(2,fromList [a1,a2])
			end
	
	fun runExperiment (render : (Tri3.triangle) dynArray * Vec3.ray3 dynArray -> Hit dynArray) (_ : string, args) = 
		let 
			val inputscene = List.nth (args,0)
			val rendersize = valOf (Int.fromString (List.nth (args,1)))
			val outputfile = List.nth (args,2)
			val size = {width=rendersize, height=rendersize}
			val params = {eye = (3.0,4.0,2.0), 
						  facing = (~0.594438,~0.63653,~0.491379), 
						  left = (~0.354306,0.75588,~0.550542), 
						  fovx = Math.pi*0.66}
			val (n,gen) = RayGenerator.Pinhole.makeRays {width=rendersize, height=rendersize} params
			val rays = (n, tabulate (n, gen))
			val geomRaw = OBJReader.readObj inputscene
			val geom : (Tri3.triangle) dynArray = (length geomRaw, fromList geomRaw)
            fun shader (g,_) = 1.0 + Real.abs (Vec3.dot (Tri3.normal g) (1.0,0.0,0.0))
			val (rSize, result) = render (geom,rays)
			
			val min = fold result rSize (fn (h,y) => case h of Depth.Finite (d,_) => Real.min (d,y) | Infty => y) Real.posInf
			val max = fold result rSize (fn (h,y) => case h of Depth.Finite (d,_) => Real.max (d,y) | Infty => y) Real.negInf
			val rcp = 255.0 / (max-min)
			fun shade h = case h of Depth.Finite (d,_) => (100,Real.floor((d - min) * rcp),100) | Infty => (0,0,255)
			val converted = fold result rSize (fn (x,y) => (shade x)::y) [] 
		in
			PpmWriter.writePicture outputfile {width = #width size, height = #height size, buffer = Vector.fromList converted};
			OS.Process.success
		end 
end
