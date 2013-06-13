
structure MbDeblocked =
struct
	
	type funcLabel = Variable.variable
	type variable = Variable.variable
	
	datatype valType    = Tgeom
						| Tray
						| Tbox
						| Tint
						| Thit
						| Tbool
						| Tarray of valType
						| Tprod of valType list
	
	datatype region 	= Elam of variable * valType * expr
	
	and expr			= Efold of string * region * expr * expr
						| Emap of string * int * region * expr
						| Elet of variable * expr * expr
						| Evar of variable
						| Etuple of expr list 
						| Eproj of int * expr
						| Einj of bool * expr
						| Eif of expr * expr * expr
						
						| Eop0 of primop0
						| Eop1 of primop1 * expr
						| Eop2 of primop2 * expr * expr
						| Eop3 of primop3 * expr * expr * expr
						
	and primop0			= P0int of int
						| P0botHit
						
	and primop1			= P1call of funcLabel
						| P1getSize
						| P1allocHitsArray
						| P1boundG
						| P1breakG of CommonSyntax.primDecompG (* changes array *)
						| P1breakS of CommonSyntax.primDecompS (* changes array *)
						
	and primop2			= P2hit 
						| P2plus
						| P2gt
						| P2closerHit
						| P2unionHits
						| P2isectBoxRay
						| P2getElement
						
	and primop3			= P3setElement
	
	datatype func		= Func of valType * funcLabel * (valType * variable) * expr
	type program = func * func list
    
end 
