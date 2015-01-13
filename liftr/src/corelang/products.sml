
signature BranchlessTypes = 
sig
	type t
	val makeprod	: t list -> t
	val unprod		: t -> t list
end
signature BranchlessValues = 
sig
	type v
	val maketuple	: v list -> v
	val untuple		: v -> v list
end

structure BranchlessFrag = 
struct
	datatype 'e exprF	= Etuple of 'e list
						| Epi of int * 'e
	datatype 't typeF	= Tprod of 't list
	datatype 'v valF	= Vtuple of 'v list
	datatype 'p pattF	= Ptuple of 'p list

	fun mapExpr	f (Etuple es) = Etuple (map f es)
	  | mapExpr f (Epi (i,e)) = Epi (i, f e)
	fun collectExpr (Etuple es) = es
	  | collectExpr (Epi (_,e)) = [e]

	functor TC (T : BranchlessTypes) = 
	struct
		fun tc (Etuple ts) = T.makeprod ts
		  | tc (Epi (i,t)) = List.nth (T.unprod t, i)
	end 

	functor Eval (V : BranchlessValues) = 
	struct
		fun eval f (Etuple es) = V.maketuple (map f es)
		  | eval f (Epi (i,e)) = List.nth (V.untuple (f e), i)
	end

end
