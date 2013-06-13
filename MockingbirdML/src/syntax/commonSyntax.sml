
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

end