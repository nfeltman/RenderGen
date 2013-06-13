
structure Runtime = struct
	
	open ListKeyBag
	open Depth
	
	exception RuntimeTypeError	
	exception NotYetImplemented	
	
	fun assertSingle bag = if (bagSize bag) = 1 then () else raise RuntimeTypeError
	
	type gsEntity = Tri3.triangle list * Vec3.ray3 keyBag
	type hitEntity = ((Tri3.triangle*(real*real)) Depth.depthed) keyBag
	type fragEntity = (real Depth.depthed) keyBag
	
	fun splitOneS samps = splitToIndividuals samps
	fun splitOneG geos = map (fn g => [g]) geos
	fun splitTwoGP (g0::rest) = [[g0],rest]
	  | splitTwoGP _ = raise RuntimeTypeError
	fun sizeG geos = List.length geos
	fun reduceG a b = mergeBags min a b
	fun reduceS a b = mergeBags (fn _ => fn _ => raise RuntimeTypeError) a b
	
	fun calcBound (l : Tri3.triangle list) = ()
	fun passesIsectTest ((bound : unit,_), l : Vec3.ray3 keyBag) = true
	fun dummy bag = mapBag (fn _ => Infty) bag
	
	fun hit (g::[],samps) = 
			let
				val _ = assertSingle samps
                val addG1 = dmap (fn uv => (g,uv))
			in
				mapBag (addG1 o (Tri3.intersectRay g)) samps
			end
	  | hit (_,_) = raise RuntimeTypeError
	
	fun mapReduce f r [] = raise RuntimeTypeError
	  | mapReduce f r (h::[]) = f h
	  | mapReduce f r (h::t) = r (f h) (mapReduce f r t)
	
	val counter = ref 0
	fun printCounter () = (print "counter is: "; print (Int.toString (!counter)); print "\n"; counter := !counter + 1)
	
	fun runExperiment (render : gsEntity -> hitEntity) = 
		let 
			val size = {width=60, height=60}
			val params = {eye = (3.0,4.0,2.0), 
						  facing = (~0.594438,~0.63653,~0.491379), 
						  left = (~0.354306,0.75588,~0.550542), 
						  fovx = Math.pi*0.66}
			val rays = ListKeyBag.genBag (RayGenerator.Pinhole.makeRays size params)
			val geom = OBJReader.readObj "../models/teapot.obj"
            fun shader (g,_) = 1.0 + Real.abs (Vec3.dot (Tri3.normal g) (1.0,0.0,0.0))
			val result = render (geom,rays)
			val converted = map (fn (d,k) => case d of Finite (d,_) => d | Infty => 0.0) result
		in
			(WriteIMG.writeStringList "../outputs/test.m" ("A="::(WriteIMG.toMatrixString 60 60 converted));map #2 result)
		end 
end
