
signature SEMILATTICE = 
sig
    type t
    val lt : t -> t -> bool
    val join : t -> t -> t
end

signature LATTICE =
sig
	type t
	
	val lt : t -> t -> bool
	val join : t -> t -> t
	val meet : t -> t -> t
end
