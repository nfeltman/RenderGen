

structure PrintMbSML = 
struct
	
	open MbSML

	fun printToFunc (p : string -> unit) program = 
		let
			val prefix = ref ""
			
			fun pLine () = (p "\n"; p (!prefix))
			and tabIn () = prefix := ((!prefix) ^ "  " )
			and tabOut () = prefix := String.extract(!prefix, 2, NONE)
			
			fun pList pSub d [] = ()
			  | pList pSub d (arg0::rest) = (pSub arg0; pListRest pSub d rest)
			and pListRest pSub d [] = ()
			  | pListRest pSub d (argN::rest) = (p d; pSub argN; pListRest pSub d rest)
			
			fun	pExprFree ex = 
				case ex of
				  Eif (e1, e2, e3) => ( tabIn();
					pLine(); p "if "; pExprFree e1; 
					pLine(); p "then "; pExprFree e2; 
					pLine(); p "else "; pExprFree e3; tabOut())
				| Etuple (vs) => pTuple vs
				| Eproj (i, v) => (p "#"; p (Int.toString (i+1)); p " ("; pExprFree v; p ")")
				| EgetElement (v1, v2) => (p "sub ("; pExprFree v1; p ", "; pExprFree v2; p ")")
				| EsetElement (v1, v2, x) => (p "setElem ("; pExprFree v1; p ", "; pExprFree v2; p ", "; pExprFree x; p ")")
				| Ecall (f, arg) => (p f; p " ("; pExprFree arg; p ")")
				| EbinOp (Bplus, v1, v2) => (pExprFree v1; p " + "; pExprFree v2)
				| EbinOp (Bgt, v1, v2) => (pExprFree v1; p " > "; pExprFree v2)
				| Evar v => p v
				| Eint i => p (Int.toString i)
				| EbotHit => p "bottomHit"
				| other => 
					(tabIn(); pLine(); 
						p "let"; tabIn(); 
						pExprLet other)
					
			and	pExprLet ex = 
				case ex of
				  Elet (x, v, rest) => 
					(pLine(); p "val "; p x;  p " = "; pExprFree v;
					pExprLet rest)
				| EfuncDefs (f, fs, rest) => 
					(pLine(); p "fun "; pFunc f; 
					List.app (fn fi => (pLine(); p "and "; pFunc fi)) fs;
					pExprLet rest)
				| other => (tabOut(); pLine(); p "in"; tabIn(); pLine(); pExprFree other; tabOut(); pLine(); p "end"; tabOut())
			
			and pFunc (name, x, ty, e) = 
				(p name; p " ("; p x; p " : "; pType ty; p ") = "; 
					tabIn(); pLine(); pExprFree e; tabOut())
			and pTuple tup = (p "("; pList pExprFree ", " tup; p ")")
			
			and pType Tgeom = p "Tri3.triangle"
			  | pType Tsamp = p "Vec3.ray3"
			  | pType Tint = p "int"
			  | pType Tbool = p "int"
			  | pType Thit = p "Hit"
			  | pType Tbox = p "box"
			  | pType (Tarray t) = (pType t; p " arr")
			  | pType (Tprod []) = p "unit"
			  | pType (Tprod l) = (p "("; pList pType " * " l; p ")")
			  | pType (Tsum (t1,t2)) = p "??????"
			  | pType (Tfix (v,t)) = p "??????"
			  | pType (Tvar v) = p "??????"
		in 
			(
				p "structure GeneratedRenderer = struct"; pLine ();
				p "open Runtime"; pLine ();
				p "val render = "; tabIn(); pLine (); 
				pExprFree program;  tabOut(); pLine ();
				p "end"; pLine ()
			)
		end
		
	val printToScreen = printToFunc print
	fun printToStream out = printToFunc (fn s => TextIO.output (out,s))
	fun printToFile outName e = 
		let
			val out = TextIO.openOut outName
		in
			printToFunc (fn s => TextIO.output (out,s)) e;
			TextIO.closeOut out
		end

end
