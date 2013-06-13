
structure RayGenerator = 
struct
	open Vec3
    
    type imageRes = {width : int, height : int}
    
    structure Pinhole =
    struct
        type params = {eye : vec3, facing : vec3, left : vec3, fovx : real}
        fun makeRays {width=w, height=h} {eye=e, facing=f0, left=l0, fovx=fovX} =
            let
                val f = normalize f0
                val l = normalize l0
                val iw = Math.tan(fovX * 0.5);
                val ih = iw * (real h) / (real w);
                val u = normalize (cross f l);
            in
                (w*h, fn i => 
                    let 
						(* need to splice in iw and ih *)
                        val px = (real (Int.div (i,h))) / (real w) * 2.0 - 1.0
                        val py = (real (Int.mod (i,h))) / (real h) * 2.0 - 1.0
                        val dir = add f (add (mul l px) (mul u py))
                    in
                        { originR = e, direction = normalize dir }
                    end)
            end
    end
end

