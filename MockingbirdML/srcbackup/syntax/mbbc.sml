

structure Mbbc =
struct
	open CommonSyntax
	
	datatype bcType	= Tray | Tbbox | Ttri 
					| Thit | Tint | Taddr 
					| Tarray of bcStruct
	and bcStruct 	= Single of bcType 
					| Compound of bcStruct * bcType
	
	type funcLabel = string
	type blockLabel = string
	type var = string
    datatype value = Vvar of var | VbotHit | Vint of int
	type labelCall = blockLabel * value list
	type branchArgs = labelCall * labelCall
	
	datatype instruction	= Iisect of var * value * value
							| Iproj of var * bcStruct * value
							| Ialloc of var * bcStruct
							| IallocArr of var * bcStruct * value
                            | Icall of var * funcLabel * value list
                            | Icloser of var * value * value
							| Ioffset of var * value * value
							| IgetElmt of var * value
							| IsetMemb of bcStruct * value * value
							| Iplus of var * value * value
							| Iset of var * value
							| Ireturn of value
							| Ijump of labelCall
							| IbranchIsect of value * value * branchArgs
							| IbranchGeq of value * value * branchArgs
							| IbranchGt of value * value * branchArgs
    
	type block = blockLabel * (bcType * var) list * instruction list
	type func = funcLabel * bcType * block list
	type program = func list
    
end 

