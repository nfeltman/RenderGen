
structure LambdaPSF = 
struct

open LangCommon

datatype ty		= Tint
				| Tbool
				| Tprod of ty list
				| Tsum of ty * ty
				| Tfunc of ty * ty
				| Tvar of int
				| Trec of ty

datatype pattern	= PPvar of var
					| PPtuple of pattern list
							
datatype 't expr	= Evar of var
					| Eint of int
					| Ebool of bool
					| Elam of 't * (pattern * 't expr)
					| Eapp of 't expr * 't expr
					| Etuple of 't expr list
					| Epi of int * 't expr
					| Einj of LR * 't * 't expr
					| Ecase of 't expr * (pattern * 't expr) * (pattern * 't expr)
					| Eif of 't expr * 't expr * 't expr
					| Elet of 't expr * (pattern * 't expr)
					| Ebinop of Prims.binops * 't expr * 't expr
					| Eroll of 't expr
					| Eunroll of 't expr
					| Eerror of 't

fun forPattern (f,_,_) g (PPvar x) t = f g x t
  | forPattern (fu as (_,unpack,ex)) g (PPtuple xs) ts = 
	let 
		fun forPatternList g (x::xs) (t::ts) = forPatternList (forPattern fu g x t) xs ts
		  | forPatternList g [] [] = g
		  | forPatternList _ _ _ = raise ex (* mismatch between pattern and product *)
	in
		forPatternList g xs (unpack ts) 
	end
					
end
