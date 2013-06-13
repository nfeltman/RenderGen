
structure LatticeOps = 
struct

structure Trivial = 
struct
	type t = unit;
	
	fun lt _ _ = true
	fun join _ _ = ()
	fun meet _ _ = ()
end

functor UpperSL (L:LATTICE) : SEMILATTICE =
struct
    type t = L.t;
	
	fun lt a b = L.lt a b
	fun join a b = L.join a b
end 

functor Flip (L:LATTICE) : LATTICE =
struct
    type t = L.t;
	
	fun lt a b = L.lt b a
	fun join a b = L.meet a b
	fun meet a b = L.join a b
end 

functor ProdSL (A:SEMILATTICE) (B:SEMILATTICE) : SEMILATTICE =
struct
    type t = A.t*B.t;
	
	fun lt (a1,b1) (a2,b2) = (A.lt a1 a2) andalso (B.lt b1 b2)
	fun join (a1,b1) (a2,b2) = (A.join a1 a2, B.join b1 b2)
end 

functor ProdL (A:LATTICE) (B:LATTICE) : LATTICE =
struct
    type t = A.t*B.t;
	
	fun lt (a1,b1) (a2,b2) = (A.lt a1 a2) andalso (B.lt b1 b2)
	fun join (a1,b1) (a2,b2) = (A.join a1 a2, B.join b1 b2)
	fun meet (a1,b1) (a2,b2) = (A.meet a1 a2, B.meet b1 b2)
end

functor CubedL (L:LATTICE) : LATTICE =
struct
    type t = L.t*L.t*L.t;
	
	fun lt (a1,b1,c1) (a2,b2,c2) = (L.lt a1 a2) andalso (L.lt b1 b2) andalso (L.lt c1 c2)
	fun join (a1,b1,c1) (a2,b2,c2) = (L.join a1 a2, L.join b1 b2, L.join c1 c2) 
	fun meet (a1,b1,c1) (a2,b2,c2) = (L.meet a1 a2, L.meet b1 b2, L.meet c1 c2)
end 

functor CubedSL (L:SEMILATTICE) : SEMILATTICE =
struct
    type t = L.t*L.t*L.t;
	
	fun lt (a1,b1,c1) (a2,b2,c2) = (L.lt a1 a2) andalso (L.lt b1 b2) andalso (L.lt c1 c2)
	fun join (a1,b1,c1) (a2,b2,c2) = (L.join a1 a2, L.join b1 b2, L.join c1 c2) 
end 

functor Func (A:LATTICE) (B:LATTICE):LATTICE = ProdL (Flip (A)) (B)

end
