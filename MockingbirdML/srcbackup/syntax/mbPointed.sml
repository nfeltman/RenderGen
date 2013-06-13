
structure MbPointed =
struct
    
    type domainType = CommonSyntax.OrderTypes.domainType
	
    datatype varType    = Tgs of domainType * domainType
						| Tint
						| Thit of domainType
    
	type funcLabel = Variable.variable
	type variable = Variable.variable
    datatype value = Vvar of Variable.variable | Vint of int
	
	datatype expr	= EforRange of variable * value * value * (varType * variable) list * expr * value list * expr (* for V in V to V {...} *)
					| Econtinue of value list
					| EallocBottomHits of variable * value * (varType * variable) list * expr
					| EallocEmptyHits of variable * expr
					| EmultiplexGeom of variable * domainType * value * value * expr
					| EmultiplexSamp of variable * domainType * value * value * expr
					| EgetGeomSize of variable * value * expr
					| EgetSampSize of variable * value * expr
                    | Ecall of variable * varType * funcLabel * value * expr
                    | EbreakG of variable * CommonSyntax.primDecompG * value * expr (* changes array *)
                    | EbreakS of variable * CommonSyntax.primDecompS * value * expr (* changes array *)
                    | EcloserHits of variable * value * value * (varType * variable) list * expr 
                    | EunionHits of variable * value * value * expr 
					| Ehit of variable * value * expr
                  (*  | EunrollG of value * domainType * label * domainType * value * expr *)
					| Eret of value
					| Eset of variable * value * expr
	
	type func = varType * funcLabel * (varType * variable) * expr
	type program = func list
    
end 
