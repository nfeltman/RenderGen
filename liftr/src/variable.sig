
signature VARIABLE =
   sig

      type variable
         
      val newvar : string -> variable
      val eq : variable * variable -> bool
	  val toString : variable -> string
	  val addSuffix : variable -> string -> variable
	  val reset : unit -> unit
	  
   end
