
structure MbDeblocked =
struct
	
    type domainType = CommonSyntax.OrderTypes.domainType
	datatype varType    = Tgeom
						| Tsamp
						| Tint
						| Thit
						| Tarray of varType list
	
	type funcLabel = Variable.variable
	type variable = Variable.variable
	datatype value = Vvar of Variable.variable | Vint of int | VbotHit
	
	datatype expr	= EforRange of variable * value * value * (varType * variable) list * expr * value list * expr (* for V in V to V {...} *)
					| Econtinue of value list
					| EallocHitsArray of variable * value * expr
					| EgetSize of variable * value * expr
                    | EgetElement of variable * varType list * value * value * int * expr
                    | EsetElement of value * value * int * value * varType list * expr   (* effectful; no return *)
                    | Ecall of variable list * funcLabel * value list * expr
                    | EbreakG of variable * CommonSyntax.primDecompG * value * expr (* changes array *)
                    | EbreakS of variable * CommonSyntax.primDecompS * value * expr (* changes array *)
                    | EcloserHit of variable * value * value * expr
                    | EunionHits of variable * value * value * expr
					| Ehit of variable * value * value * expr
					| Eret of value list
					| Eset of variable * value * expr
                    | Eplus of variable * value * value * expr
	
	type func = varType list * funcLabel * (varType * variable) list * expr
	type program = func list
    
end 
