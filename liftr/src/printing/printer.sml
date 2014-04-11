
structure PrettyPrinter = 
struct

datatype prioSlot = Pliteral of string | Psubterm of int * prioTerm
withtype prioTerm = int * prioSlot list

datatype hierSlot = Hliteral of string | Hsubterm of hierTerm
withtype hierTerm = hierSlot list

fun resolvePrioTerm slotLevel (termLevel,subs) = 
	if termLevel > slotLevel 
	then [Hliteral "(", Hsubterm (map resolvePrioSlot subs), Hliteral ")"]
	else map resolvePrioSlot subs
	
and resolvePrioSlot (Pliteral s) = Hliteral s
  | resolvePrioSlot (Psubterm (slotLevel,e)) = Hsubterm(resolvePrioTerm slotLevel e)

fun printTerm p es = 
	app (fn e => case e of Hliteral l => p l | Hsubterm s => printTerm p s) es
  
end
