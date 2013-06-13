
structure MbDetupled =
struct
	
	type funcLabel = Variable.variable
	type variable = Variable.variable
    
	datatype domainType = Dflat
						| Darray of domainType
	
	datatype valType    = Tgeom of domainType
						| Tsamp of domainType (* contains its key implicitly *)
						| Tint
						| Thit of domainType  (* contains its key implicitly *)
						
	datatype region 	= Rdef of valType list * (valType * variable) list * expr
	
		and		 expr	= EforRange of variable list * expr * expr * region * expr list * expr (* for V in V to V {...} *)
						| EallocBottomHits of expr
						| EallocEmptyHits
						| EgetSize of expr
						| EgetElement of valType * expr * expr
						| Ecall of funcLabel * expr list
						| EbreakG of CommonSyntax.primDecompG * expr (* changes array *)
						| EbreakS of CommonSyntax.primDecompS * expr (* changes array *)
						| EcloserHits of expr * expr  (* consumes first one *)
						| EunionHits of expr * expr
						| Ehit of expr * expr
						| Elet of variable * expr * expr
						| Evar of variable
						| Eint of int
	
	datatype func		= Func of valType list * funcLabel * (valType * variable) list * expr
	type program = func * func list
    
end 
