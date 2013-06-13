
structure MbOrder =
struct
	open CommonSyntax
    open CommonSyntax.OrderTypes
	
    datatype MS = Multi | Single
    
    datatype decG	= Dflat
                    | DsizeCase of int * decG * decG
                    | Dbound of decG
                    | Dlayer of primDecompG * decG
                    | Dfix of label * domainType * decG
                    | Dlabel of label
						
	datatype expr	= Echain of stageType * expr * expr
                    | Emmr of caseDomain * expr
                    | EsizeCase of caseDomain * int * expr * expr
                    | ErememberCase of expr * expr
                    | Etest of expr
                    | EfiltS of expr
                    | EbreakG of domainType * decG
                    | EbreakS of domainType * primDecompS
                    | Ehit
                    | Eshade
                    | EboundG of domainType * MS
                    | EunboundG of domainType * MS
                    | EboundS of domainType * MS
                    | EunboundS of domainType * MS
                    | Efix of label * exprType * expr
                    | Elabel of label
					| EunrollG of domainType * label * domainType
					
					(* | EassertType of exprType * expr *)
    
end 
