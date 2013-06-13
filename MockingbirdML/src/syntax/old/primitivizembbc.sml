
structure Primitivize =
struct
	open CommonSyntax
	structure P = MbDeblocked
	open Mbbc
    
	exception NotImplemented
	exception TypeMismatch
	exception ContinueOutsideLoop
	
	val map = List.map
	
	fun lookupType liveVars (P.Vvar v) = (case Util.find liveVars v of SOME a => a | NONE => raise NotImplemented)
	  | lookupType _ (P.Vint _) = P.Tint
	  | lookupType _ P.VbotHit = P.Thit
	
	fun getGBreakFunction OneG = "performOneG"
	  | getGBreakFunction _ = raise NotImplemented
	fun getSBreakFunction OneS = "performOneS"
	  | getSBreakFunction _ = raise NotImplemented
	  
	fun trVar var = Variable.toString var
	fun trVal (P.Vvar var) = Vvar (trVar var)
	  | trVal (P.Vint i) = Vint i
	  | trVal P.VbotHit = VbotHit
	fun getVar s = trVar (Variable.newvar s)
	
	fun trType P.Tgeom = Ttri
	  | trType P.Tsamp = Tray
	  | trType P.Tint = Tint
	  | trType P.Thit = Thit
	  | trType (P.Tarray _) = Taddr
	
	fun trArg (ty,name) = (trType ty, trVar name)
		
	and trExpression addBlock  blockSource onContinue (liveVars : (P.variable*P.valType) list) expr =
		let 
			val translateExpr = trExpression addBlock blockSource onContinue
			fun trExpr rest v = translateExpr (v::liveVars) rest 
		in
			case expr of 
			  P.EforRange (sinks, startI, endI, P.Rdef (outTypes, inArgs, exp), init, rest) => 
				let
					val (iter, extraArgs) = 
							case inArgs of
							  (P.Tint,x)::xs => (trVar x, xs)
							| _ => raise TypeMismatch
					
					val iter2 = getVar "iterNext"
					
					val headerLabel = getVar "header"
					val bodyLabel = getVar "body"
					val exitLabel = getVar "exit"
					val finalLoopBlock = ref "????????" (* gets set below *)
					val finalLoopArgs = ref [] (* gets set below *)
					
					(* body *)
					fun subContinue source vals = (finalLoopBlock := source ; finalLoopArgs := vals; [
						Iplus (iter2, Vvar iter, Vint 1), 
						Ijump headerLabel])
					val body = (bodyLabel, trExpression addBlock bodyLabel subContinue liveVars exp)
					
					
					(* header *)
					val loopAccum = Util.zip extraArgs (Util.zip init (!finalLoopArgs))
					fun trExtraArgs ((ty,name),(initVal,recVal)) = 
							Iphi (trVar name, trType ty, (blockSource, trVal initVal), (!finalLoopBlock, recVal), [])
					val header = (headerLabel, 
						Iphi (iter, Tint, (blockSource, trVal startI), (!finalLoopBlock, Vvar iter2), []) :: 
						(map trExtraArgs loopAccum) @
						[IbranchGt (trVal endI, Vvar iter, (bodyLabel, exitLabel))])
					
					(* exit *)
					val finalresult = (Util.zip sinks outTypes) @ liveVars
					val sinkSetters = map (fn (sink,(_,name)) => Iset(trVar sink, Vvar (trVar name))) (Util.zip sinks extraArgs)
					val exit = (exitLabel, sinkSetters @ (trExpression addBlock exitLabel onContinue finalresult rest))
				in
					(addBlock exit; addBlock body; addBlock header; [Ijump headerLabel])
				end
			  
			  
			| P.Econtinue vals => onContinue blockSource (map trVal vals) 
			| P.EallocHitsArray (sink, size, rest) => 
				let
					val arr = getVar "arr"
					val newSink = trVar sink
				in
					Ialloc (newSink, Compound (Single Tint, Tarray (Compound (Single Tint, Thit)))) :: 
					IsetMemb (Single Tint, Vvar newSink, trVal size) :: 
					IallocArr (arr, Compound (Single Tint, Thit), trVal size) :: 
					IsetMemb (Compound (Single Tint, Tarray (Compound (Single Tint, Thit))), Vvar newSink, Vvar arr) :: 
					trExpr rest (sink,P.Tarray [P.Thit])
				end
			| P.EgetSize (sink, arr, rest) =>
				Iproj (trVar sink, Single Tint, trVal arr) ::
				trExpr rest (sink,P.Tint)
			| P.EgetElement (types, sink, arrSt, i, subIndex, rest) =>
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
					trExpr rest (sink, List.nth (types,subIndex))
				end
			| P.EsetElement (types, arrSt, i, subIndex, x, rest) => 
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
					translateExpr liveVars rest
				end
			| P.Ecall ([sink], label, args, rest) => 
				Icall (trVar sink, trVar label, map trVal args) ::
				trExpr rest (sink,P.Tint) (* THIS IS NOT CORRECT *)
			| P.Ecall _ => raise NotImplemented
			| P.EbreakG (sink, dec, x, rest) =>
				Icall (trVar sink, getGBreakFunction dec, [trVal x]) :: 
				trExpr rest (sink,P.Tarray [P.Tgeom])
			| P.EbreakS (sink, dec, x, rest) => 
				Icall (trVar sink, getSBreakFunction dec, [trVal x]) :: 
				trExpr rest (sink,P.Tarray [P.Tsamp])
			| P.EunionHits (sink, h1, h2, rest) => 
				Icall (trVar sink, "unionHits", [trVal h1, trVal h2]) :: 
				trExpr rest (sink,P.Thit)
			| P.Ehit (sink, g, s, rest) =>
				Iisect (trVar sink, trVal g, trVal s) ::
				trExpr rest (sink,P.Thit)
			| P.EcloserHit (sink, h1, h2, rest) =>
				Icloser (trVar sink, trVal h1, trVal h2) ::
				trExpr rest (sink,P.Thit)
			| P.Eret [v] => [Ireturn (trVal v)]
			| P.Eret _ => raise NotImplemented
			| P.Eset (sink, v, rest) => 
				Iset (trVar sink, trVal v) ::
				trExpr rest (sink,lookupType liveVars v)
			| P.Eplus (sink, v1, v2, rest) =>
				Iplus (trVar sink, trVal v1, trVal v2) ::
				trExpr rest (sink,P.Tint)
		end
	and convertPrefix (t::ts) n = 
		let 
			fun subConvert acc _ 0 = acc
			  | subConvert acc (t::ts) n = subConvert (Compound (acc, trType t)) ts (n-1)
			  | subConvert _ _ _ = raise NotImplemented  
		in
			subConvert (Single (trType t)) ts n
		end
	  | convertPrefix _ _ = raise NotImplemented
	
	and convertAll t = convertPrefix t ((List.length t) - 1)
	
	
	
	  
	and trJumpInfo (blockLabel, args) = (blockLabel, map trVal args)
	and trBranchInfo (j1, j2) = (trJumpInfo j1, trJumpInfo j2)
	
	
	fun trFunction (P.Func ([outType], name, inputs, rootExpr)) = 
		let 			
			val blocks = ref []
			val entryBlock = "entry"
			fun addBlock z = (blocks := z :: !blocks)
			val trRoot = trExpression addBlock entryBlock (fn _ => raise ContinueOutsideLoop) [] rootExpr
		in
			(trVar name, trType outType, ((entryBlock, trRoot) :: !blocks )) : func
		end
	  | trFunction (P.Func _) = raise NotImplemented
	
	fun translate program = map trFunction program
end 
