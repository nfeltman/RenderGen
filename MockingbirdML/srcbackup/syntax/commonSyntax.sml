
structure CommonSyntax = 
struct
	(* types *)
	datatype 'a stage	= GeoSamps of 'a * 'a 
                        | Hits of 'a
                        | Frags of 'a
	
	datatype decompPurity = GenDecomp | KeyDecomp
	datatype caseDomain = GeoCase | SampCase | HitCase | FragCase
	
	(* terms *)
	datatype mergeMethod = General | Union
	datatype primDecompG = OneG | TwoGP
    datatype primDecompS = OneS | SixteenSqSP
    datatype primDecompH = OneH
	datatype decomp = PrimG of primDecompG
                    | PrimS of primDecompS
                    | PrimH of primDecompH
                    
    structure OrderTypes = struct
        type label = Variable.variable
	
        datatype domainType = Tflat
                            | Tarray of domainType
                            | Tsum of domainType * domainType
                            | Tbounded of domainType
                            | Tfix of label * domainType
                            | Tvar of label
        
        type stageType = domainType stage
        type exprType = stageType * stageType
    end
end












