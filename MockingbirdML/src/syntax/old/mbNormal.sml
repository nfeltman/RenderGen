
structure MbNormal =
struct
	open CommonSyntax
    open CommonSyntax.OrderTypes
    
	type funcLabel = Variable.variable
    datatype lr = LEFT | RIGHT
	datatype expr	= Echain of stageType * expr * expr
					| Emmr of caseDomain * stageType * expr
                    | Ecall of funcLabel * stageType
                    | EsizeCase of caseDomain * int * expr * expr
                    | ErememberCase of expr * expr
                    | Etest of expr
                    | EboundG
                    | EunboundG
                    | EpreBoundGStruct of expr
                    | EmapBuildG of expr * expr
                    | EfiltS of expr
                    | EbreakG of primDecompG
                    | EbreakS of primDecompS
                    | Einj of lr
					| Ehit
					| Eshade
                    | EunrollG of domainType * label * domainType
					| Eid
	
    type func = funcLabel * exprType * expr
	type program = func * func list
    
end 
