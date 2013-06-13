

structure PrintMBBC = 
struct
	
	open Mbbc

	fun printToFunc p program = 
		let
			fun printProgram prog = (p "generated: "; List.app pFunction prog)
			and pFunction (name, t, blocks) = (pType t; p " "; p name; p "\n{\n"; map pBlock blocks; p "}\n")
			and pBlock (name, body) = (p name; p " :"; map pInstr body; p "\n\n")
			and pInstr inst = ( p "\n\t"; (
					case inst of 
					  Iisect (var, arg1, arg2) => (pSetter var; p "isect "; pValue2 arg2 arg1)
					| Iproj (var, st, arg) => (pSetter var; p "proj "; pStruct st; pValue arg)
					| Ialloc (var, st) => (pSetter var; p "alloc "; pStruct st)
					| IallocArr (var, st, arg) => (pSetter var; p "allocArr "; pStruct st; pValue arg)
					| Icall (var, name, args) => (pSetter var; p "call "; p name; pArgList args)
					| Icloser (var, arg1, arg2) => (pSetter var; p "closer "; pValue2 arg1 arg2)
					| Ioffset (var, arg1, arg2) => (pSetter var; p "offset "; pValue2 arg1 arg2)
					| IgetElmt (var, arg) => (pSetter var; p "getElmt "; pValue arg)
					| IsetMemb (st, arg1, arg2) => (p "setMemb "; pStruct st; pValue2 arg1 arg2)
					| Iplus (var, arg1, arg2) => (pSetter var; p "plus "; pValue2 arg1 arg2)
					| Iset (var, arg) => (pSetter var; pValue arg)
					| Ireturn (arg) => (p "ret "; pValue arg)
					| Ijump call => (p "jump "; p call)
					| Iphi (var, ty, (b1,a1), arg2, argRest) => (pSetter var; p "phi "; pType ty; p " "; pValue a1;  p " "; p b1; pPhiArg arg2; List.app pPhiArg argRest)
					| IbranchIsect (arg1, arg2, branch) => (p "branch_isect "; pValue2 arg1 arg2; pBranchCall branch)
					| IbranchGeq (arg1, arg2, branch) => (p "branch_geq "; pValue2 arg1 arg2; pBranchCall branch)
					| IbranchGt (arg1, arg2, branch) => (p "branch_gt "; pValue2 arg1 arg2; pBranchCall branch)
					); p ";")
			and pSetter var = (p var; p " = ")
			and pValue (Vvar v) = (p v)
			  | pValue VbotHit = p "BOTTOM"
			  | pValue (Vint i) = (p (Int.toString i))
			and pValue2 val1 val2 = (pValue val1; p " "; pValue val2)
			and pBranchCall (call1,call2) = (p " "; p call1; p " "; p call2)
			and pArgList [] = p "()"
			  | pArgList (arg0::rest) = (p "("; pValue arg0; pArgRest rest)
			and pArgRest [] = p ")"
			  | pArgRest (argN::rest) = (p ", "; pValue argN; pArgRest rest)
			and pParamList [] = p "()"
			  | pParamList ((t,arg0)::rest) = (p "("; pType t; p " "; p arg0; pParamRest rest)
			and pParamRest [] = p ")"
			  | pParamRest ((t,argN)::rest) = (p ", "; pType t; p " "; p argN; pParamRest rest)
			and pPhiArg (blockLabel, value) = (p ", "; pValue value;  p " "; p blockLabel)
			and pType Tray = p "Ray"
			  | pType Tbbox = p "BBox"
			  | pType Ttri = p "Tri"
			  | pType Thit = p "Hit"
			  | pType Tint = p "Int"
			  | pType Taddr = p "StructAddr"
			  | pType (Tarray st) = (p "ArrayPtr "; pStruct st)
			and pStruct st = (pStructRest st; p "} ")
			and pStructRest (Single t) = (p "{"; pType t)
			  | pStructRest (Compound (st, t)) = (pStructRest st; p " "; pType t)
		
		in
			printProgram program
		end
		
	val printToScreen = printToFunc print

end
