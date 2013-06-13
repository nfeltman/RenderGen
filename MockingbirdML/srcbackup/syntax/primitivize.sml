
structure Primitivize =
struct
	open CommonSyntax
	structure P = MbDeblocked
	open Mbbc
    
	exception NotImplemented
	exception ContinueOutsideLoop
	
	val map = List.map
	
	(*
	
	
	
	*)
	
	fun translate program = 
		let
			fun getGBreakFunction OneG = "performOneG"
			  | getGBreakFunction _ = raise NotImplemented
			fun getSBreakFunction OneS = "performOneS"
			  | getSBreakFunction _ = raise NotImplemented
			
			fun trFunction ([outType], name, inputs, rootExpr) = 
				let 
					
					val blocks = ref []
					fun addBlock z = (blocks := z :: !blocks)
					
					fun convertType P.Tgeom = Ttri
					  | convertType P.Tsamp = Tray
					  | convertType P.Tint = Tint
					  | convertType P.Thit = Thit
					  | convertType (P.Tarray _) = Taddr
					fun convertArg (ty,name) = (convertType ty, trVar name)
					
					and convertPrefix (t::ts) n = 
						let 
							fun subConvert acc _ 0 = acc
							  | subConvert acc (t::ts) n = subConvert (Compound (acc, convertType t)) ts (n-1)
							  | subConvert _ _ _ = raise NotImplemented  
						in
							subConvert (Single (convertType t)) ts n
						end
					  | convertPrefix _ _ = raise NotImplemented
					
					and convertAll t = convertPrefix t ((List.length t) - 1)
					
					and trExpr expr onContinue piped =
							case expr of 
							  P.EforRange (i, startI, endI, closed, insides, call, rest) => 
								let
									
									val iter = trVar i
									val itermax = getVar "itermax"
									
									val headerLabel = getVar "header"
									val bodyLabel = getVar "body"
									val exitLabel = getVar "exit"
									
									val convertedClosed = (map convertArg closed) @ piped
									val args = (Tint,iter) :: (Tint,itermax) :: convertedClosed
																	
									(* preheader *)
									val invariantAddr = Variable.newvar "invariantDomain"
									val preheader =[Iset (iter, trVal startI),
													Iset (itermax, trVal endI),
													Ijump (headerLabel, Vvar iter :: Vvar itermax :: (map trVal call) @ (map (Vvar o #2) piped))]
									
									(* header *)
									val header = (headerLabel, args, 
											[IbranchGeq (Vvar iter,Vvar itermax,
												((bodyLabel, map (Vvar o #2) args), (exitLabel, map (Vvar o #2) convertedClosed)))])
									
									(* body *)
									val iter2 = getVar "iterNext"
									fun subContinue vals = [
										Iplus (iter2, Vvar iter, Vint 1),
										Ijump (headerLabel, vals)]
									val body = (bodyLabel, args, trExpr insides subContinue args)
									
									(* exit *)
									val exit = (exitLabel, convertedClosed, trExpr rest onContinue piped)
								in
									(addBlock exit; addBlock body; addBlock header; preheader)
								end
							  
							  
							| P.Econtinue vals => onContinue (map trVal vals) 
							| P.EallocHitsArray (sink, size, rest) => 
								IallocArr (trVar sink, Compound (Single Tint, Tray), trVal size) :: 
								trExpr rest onContinue piped
							| P.EgetSize (sink, arr, rest) =>
								Iproj (trVar sink, Single Tint, trVal arr) ::
								trExpr rest onContinue piped
							| P.EgetElement (sink, types, arrSt, i, subIndex, rest) =>
								let 
									val ty = Single Taddr
									val arr = getVar "arr"
									val moved = getVar "indexed"
									val st = getVar "asStruct"
								in
									Iproj (arr, Compound (Single Tint, Tarray (convertAll types)), trVal arrSt) :: 
									Ioffset (moved, Vvar arr, trVal i) :: 
									IgetElmt (st, Vvar moved) :: 
									Iproj (trVar sink, convertPrefix types subIndex, Vvar st) :: 
									trExpr rest onContinue piped
								end
							| P.EsetElement (arrSt, i, subIndex, x, types, rest) => 
								let 
									val ty = Single Taddr
									val arr = getVar "arr"
									val moved = getVar "indexed"
									val st = getVar "asStruct"
								in
									Iproj (arr, Compound (Single Tint, Tarray (convertAll types)), trVal arrSt) :: 
									Ioffset (moved, Vvar arr, trVal i) :: 
									IgetElmt (st, Vvar moved) :: 
									IsetMemb (convertPrefix types subIndex, Vvar st, trVal x) :: 
									trExpr rest onContinue piped
								end
							| P.Ecall ([sink], label, args, rest) => 
								Icall (trVar sink, trVar label, map trVal args) ::
								trExpr rest onContinue piped
							| P.Ecall _ => raise NotImplemented
							| P.EbreakG (sink, dec, x, rest) =>
								Icall (trVar sink, getGBreakFunction dec, [trVal x]) :: 
								trExpr rest onContinue piped
							| P.EbreakS (sink, dec, x, rest) => 
								Icall (trVar sink, getSBreakFunction dec, [trVal x]) :: 
								trExpr rest onContinue piped
							| P.EunionHits (sink, h1, h2, rest) => 
								Icall (trVar sink, "unionHits", [trVal h1, trVal h2]) :: 
								trExpr rest onContinue piped
							| P.Ehit (sink, g, s, rest) =>
								Iisect (trVar sink, trVal g, trVal s) ::
								trExpr rest onContinue piped
							| P.EcloserHit (sink, h1, h2, rest) =>
								Icloser (trVar sink, trVal h1, trVal h2) ::
								trExpr rest onContinue piped
							| P.Eret [v] => [Ireturn (trVal v)]
							| P.Eret _ => raise NotImplemented
							| P.Eset (sink, v, rest) => 
								Iset (trVar sink, trVal v) ::
								trExpr rest onContinue piped
							| P.Eplus (sink, v1, v2, rest) =>
								Iplus (trVar sink, trVal v1, trVal v2) ::
								trExpr rest onContinue piped
												
								
					and trVal (P.Vvar var) = Vvar (trVar var)
					  | trVal (P.Vint i) = Vint i
					  | trVal P.VbotHit = VbotHit
					and trVar var = Variable.toString var
					and getVar s = trVar (Variable.newvar s)
					
					
					  
					and trJumpInfo (blockLabel, args) = (blockLabel, map trVal args)
					and trBranchInfo (j1, j2) = (trJumpInfo j1, trJumpInfo j2)
					
					val trRoot = trExpr rootExpr (fn _ => raise ContinueOutsideLoop) []
				in
					(trVar name, convertType outType, (("entry", map (fn (t,a) => (convertType t, trVar a)) inputs, trRoot) :: !blocks )) : func
				end
			| trFunction _ = raise NotImplemented
		in
			map trFunction program
		end
end 
