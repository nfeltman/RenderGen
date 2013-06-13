
structure MbFilter =
struct
	open CommonSyntax
	
    structure FilterSimple =
    struct
	datatype 'r block	= EChain of 'r * 'r
                        | EMerge of mergeMethod * decomp * 'r
                        | ESizeCase of caseDomain * int * 'r * 'r
                        | ETest of 'r
                        | EHit
                        | EShade
    end
    
    structure FilterFP = FixedPointSyntax (FilterSimple) 
end 












