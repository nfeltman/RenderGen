
structure MbPointed =
struct
    
	type funcLabel = Variable.variable
	type variable = Variable.variable
	
	datatype boundType	= Bray
						| Bbox
		
    datatype valType    = TgeomsFlat
						| TsampsFlat
						| Tint
						| Tbool
						| ThitsFlat
						| Tbound of boundType
						| Tarray of valType
						| Tprod of valType list
						| Tsum of valType * valType
						| Tfix of variable * valType
						| Tvar of variable
	
	datatype region 	= Elam of variable * valType * expr
	
	and	expr			= Efold of string * region * expr * expr
						| Emap of string * int * region * expr
						| Eif of expr * expr * expr
						| Elet of variable * expr * expr
						| Evar of variable
						| Ecall of funcLabel * expr
						| Einj of bool * expr
						| Ecase of expr * variable * expr * variable * expr
						| Etuple of expr list
						| Eproj of int * expr
						| Eunroll of variable * valType * expr
						| Eroll of variable * valType * expr
						
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
