
structure MbPointed =
struct
    
	type funcLabel = Variable.variable
	type variable = Variable.variable
	
	datatype boundType	= Bray
						| Bbox
	
	datatype domainType = Dflat
						| Dbounded of boundType * domainType
						| Darray of domainType
	
    datatype valType    = Tgeoms of domainType
						| Tsamps of domainType
						| Tint
						| Tbool
						| Thit of domainType
						| Tprod of valType list
	
	datatype region 	= Elam of variable * valType * expr
	
	and	expr			= Efold of string * region * expr * expr
						| Emap of string * int * region * expr
						| Eif of expr * expr * expr
						| Elet of variable * expr * expr
						| Evar of variable
						| Ecall of funcLabel * expr
						| Einj of bool * expr
						| Etuple of expr list
						| Eproj of int * expr
						
						| Eisect of expr * expr
						| EsizeGt of expr * int
						| EallocBottomHits of expr
						| EbreakG of CommonSyntax.primDecompG * expr (* changes array *)
						| EbreakS of CommonSyntax.primDecompS * expr (* changes array *)
						| EboundG of expr
						| EboundS of expr
						| EcloserHits of expr * expr
						| EunionHits of expr * expr
						| Ehit of expr * expr
						| Eint of int
						| EallocEmptyHits
		
	datatype func 		= Func of valType * funcLabel * (valType * variable) * expr
	type program = func list
    
end 
