
structure Variable : VARIABLE =
   struct

      type variable = int * string

      val nextvar = ref 0
      fun newvar s =
          let val v = !nextvar
          in
             nextvar := v+1;
             (v,s)
          end

      fun eq ((v1,_) : variable, (v2,_)) = v1 = v2
	  
	  fun toString (v,s) = s^"_"^(Int.toString v)
	  
	  fun addSuffix (_,s) suff = newvar (s ^ suff)

   end
