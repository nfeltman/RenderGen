
structure MbDetupled =
struct
	
    type domainType = CommonSyntax.OrderTypes.domainType
	datatype varType    = Tgeom of domainType
						| Tsamp of domainType (* contains its key implicitly *)
						| Tint
						| Thit of domainType  (* contains its key implicitly *)
	
	type funcLabel = Variable.variable
	type variable = Variable.variable
	datatype value = Vvar of Variable.variable | Vint of int
	
	datatype expr	= EforRange of variable * value * value * (varType * variable) list * expr * value list * expr (* for V in V to V {...} *)
					| Econtinue of value list
					| EallocBottomHits of variable * value * (varType * variable) list * expr
					| EallocEmptyHits of variable * expr
					| EgetSize of variable * value * expr
                    | EgetElement of variable * varType * value * value * expr
            (*        | EsetElement of value * value * value * varType * expr   (* effectful; no return *) *)
                    | Ecall of variable list * funcLabel * value list * expr
                    | EbreakG of variable * CommonSyntax.primDecompG * value * expr (* changes array *)
                    | EbreakS of variable * CommonSyntax.primDecompS * value * expr (* changes array *)
                    | EcloserHits of variable * value * value * (varType * variable) list * expr  (* consumes first one *)
                    | EunionHits of variable * value * value * expr
					| Ehit of variable * value * value * expr
					| Eret of value list
					| Eset of variable * value * expr
	
	type func = varType list * funcLabel * (varType * variable) list * expr
	type program = func list
    
end 
