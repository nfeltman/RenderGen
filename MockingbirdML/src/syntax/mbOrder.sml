
structure MbOrder =
struct
	open CommonSyntax

	type label = Variable.variable

	datatype domainType = Tflat
						| Tarray of domainType
						| Tsum of domainType * domainType
						| Tbounded of domainType
						| Tfix of label * domainType
						| Tvar of label
	
	type stageType = domainType stage
	type exprType = stageType * stageType
	
	datatype decG	= Dflat
					| DsizeCase of int * decG * decG
					| Dbound of decG
					| Dlayer of primDecompG * decG
					| Dfix of label * domainType * decG
					| Dlabel of label

	datatype expr	= Echain of expr * expr
					| Emmr of caseDomain * expr
					| EsizeCase of caseDomain * int * expr * expr
					| ErememberCase of expr * expr
					| Etest of expr
					| EfiltS of expr
					| EbreakG of domainType * decG
					| EbreakS of domainType * primDecompS
					| Ehit
					| Eshade
					| EunboundG of domainType * domainType
					| EboundS of domainType
					| EunboundS of domainType * domainType
					| Efix of label * exprType * expr
					| Elabel of label
					| EunrollG of domainType * label * domainType

	(* | EassertType of exprType * expr *)

end 
