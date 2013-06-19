

structure MbSML =
struct
	type funcLabel = string
	type variable = string
	
	datatype valType    = Tgeom
						| Tsamp
						| Tint
						| Thit
						| Tbool
						| Tbox
						| Tarray of valType
						| Tprod of valType list
						| Tsum of valType * valType
						| Tvar of variable
	
	datatype value 		= Vvar of variable 
	
	
	datatype expr		= EfuncDefs of func * func list * expr
						| Evar of variable
						| Eif of expr * expr * expr
						| Etuple of expr list
						| Eproj of int * expr
						| Ecase of expr * variable * expr * variable * expr
						| EgetElement of expr * expr
						| EsetElement of expr * expr * expr
						| Ecall of funcLabel * expr
						| Elet of variable * expr * expr
						| EbinOp of binOp * expr * expr
						| Eint of int 
						| EbotHit
	
	and binOp			= Bplus
						| Bgt
						
	withtype func		= funcLabel * variable * valType * expr
	and 	 recdec		= variable * variable * valType
end 

