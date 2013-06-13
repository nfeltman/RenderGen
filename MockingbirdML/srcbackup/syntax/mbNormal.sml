
structure MbNormal =
struct
	open CommonSyntax
    open CommonSyntax.OrderTypes
    
	type funcLabel = Variable.variable
    datatype lr = LEFT | RIGHT
	datatype expr	= Emmr of caseDomain * expr * stageType * expr
                    | Eemit
                    | Ecall of funcLabel * stageType * expr
                    | EsizeCase of caseDomain * int * expr * expr * expr
                    | ErememberCase of expr * expr * expr
                    | Etest of expr
                    | EboundG of expr
                    | EunboundG of expr
                    | EpreBoundGStruct of expr * expr
                    | EmapBuildG of expr * expr
                    | EfiltS of expr
                    | EbreakG of primDecompG * expr
                    | EbreakS of primDecompS * expr
                    | Einj of lr * expr
					| Ehit of expr
					| Eshade of expr
                    | EunrollG of domainType * label * domainType * expr
	
    type func = funcLabel * exprType * expr
	type program = func list
    
end 
