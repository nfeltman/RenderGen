structure Frag = 
struct
	type ('e,'b) exprF
	type 't typeF
	type ('v,'e) valF
	type 'p pattF

	val mapExpr	: ('e1 -> 'e2) -> ('b1 -> 'b2) -> ('e1,'b1) expr -> ('e2,'b2) expr
	val mapType	: ('t1 -> 't2) -> 't1 typeF -> 't2 typeF

end

signature ExprMap = 
sig
	type ('e,'b) expr
	type r
	val m : ('e -> r) -> ('b * r -> r) -> ('e,'b) exprF -> r
end

structure ProductFragment = 
struct
	datatype 'e exprF	= Etuple of 'e list
						| Epi of int * 'e
	datatype 't typeF	= Tprod of 't list
	datatype 'v valF	= Vtuple of 'v list
	datatype 'p pattF	= Ptuple of 'p list

	fun mapExpr	f (Etuple es) = Etuple (map f es)
	  | mapExpr f (Epi (i,e)) = Epi (i, f e)

	functor TC (T : ProductTypes) = 
	struct
		fun tc (Etuple ts) = T.makeprod ts
		  | tc (Epi (i,t)) = List.nth (T.unprod t, i)
	end 

	functor Eval (V : ProductValues) = 
	struct
		fun eval f (Etuple es) = V.maketuple (map f es)
		  | eval f (Epi (i,e)) = List.nth (V.untuple (f e), i)
	end

end