
structure NormalToSML =
struct
	open CommonSyntax
	open MbNormal
	
	exception NotImplemented
    
	fun p s = print s
	fun pl s = (print s; print "\n")
	
	
	fun domainToLetter GeoCase = "G"
	  | domainToLetter SampCase = "S"
	  | domainToLetter HitCase = "H"
	  | domainToLetter FragCase = "F"
	fun gSplitFuncName OneG = "splitOneG"
	  | gSplitFuncName TwoGP = "splitTwoGP"
	fun sSplitFuncName OneS = "splitOneS"
	  | sSplitFuncName SixteenSqSP = "splitSixteenSqSP"
	
	datatype mode = REDUCE | RETURN
	fun printExp e m =
		case e of
		  EEmit => pl "x"
		| EMapReduce (GeoCase, e1, e2) => 
				(
				pl "let val s = #2 x ";
				pl "fun f g = let val x = (g,s) in "; printExp e1 RETURN; pl "end";
				pl "val x = mapReduce f reduceG (#1 x) in"; 
				printExp e2 m;
				pl "end"
				)
		| EMapReduce (SampCase, e1, e2) => 
				(
				pl "let val g = #1 x ";
				pl "fun f s = let val x = (g,s) in "; printExp e1 RETURN; pl "end";
				pl "val x = mapReduce f reduceS (#2 x) in"; 
				printExp e2 m;
				pl "end"
				)
		| EMapReduce (_,_,_) => raise NotImplemented
		| ECall (s, e) => (p "let val x = "; p s; pl " x in"; printExp e m; pl "end")
		| ESizeCase (d, i, e1, e2) =>	
				(
				p "if size"; p (domainToLetter d); p " (#1 x) >= "; p (Int.toString i); 
				pl " then";
				printExp e1 m;
				pl "else";
				printExp e2 m
				)
		| ERememberCase (e1, e2) =>	
				(
				pl "case x of \n  (LeftCase g,s) => ";
				pl "let val x = (g,s) in";
				printExp e1 m;
				pl "end";
				pl "| (RightCase g, s) => ";
				pl "let val x = (g,s) in";
				printExp e2 m;
				pl "end"
				)
		| ETest e =>	
				(
				pl "if passesIsectTest x then ";
				pl "let val x = (#2 (#1 x), #2 x) in ";
				printExp e m;
				pl "end";
				pl "else dummy (#2 x)"
				)
		| EBound e => (pl "let val x = ((calcBound (#1 x), #1 x), #2 x) in "; printExp e m; pl "end") 
		| EPreBoundGStruct (e1, e2) =>	
				(
				pl "let val bound = calcBound (#1 x) "; 
				pl "val (substruct,s) = "; printExp e1 RETURN;
				pl "val x = ((bound,substruct), s) in"; 
				printExp e2 m;
				pl "end"
				)
		| EMapBuildG (e1, e2) => 
				(
				pl "let val s = #2 x ";
				pl "fun f g = let val x = (g,s) in #1 ("; printExp e1 RETURN; pl ") end";
				pl "val x = (map f (#1 x), s) in"; 
				printExp e2 m;
				pl "end"
				)
		| EFilt e => (pl "let val (x, filtered) = x "; pl "val solved = "; printExp e m; pl "in filterMerge solved filtered"; pl "end")
		| EBreakG (prim, e) => (p "let val x = ("; p (gSplitFuncName prim); pl " (#1 x), #2 x) in "; printExp e m; pl "end")
		| EBreakS (prim, e) => (p "let val x = (#1 x, "; p (sSplitFuncName prim); pl " (#2 x)) in "; printExp e m; pl "end")
		| EInj (LEFT, e) => (pl "let val x = (LeftCase (#1 x), #2 x) in "; printExp e m; pl "end")
		| EInj (RIGHT, e) => (pl "let val x = (RightCase (#1 x), #2 x) in "; printExp e m; pl "end") 
		| EHit e => (pl "let val x = hit x in "; printExp e m; pl "end")
		| EShade e => (pl "let val x = shade x in "; printExp e m; pl "end")
		
	
	
	fun printFunction (name, body) = (
									 p "and "; p name; pl " x = ";
									 printExp body RETURN;
									 pl ""
									 )
									 
	fun printProgram prog = (
							pl "structure GeneratedRenderer = struct";
							pl "open Runtime";
							pl "fun go () = runExperiment root";
							map printFunction prog;
							pl "end"
							)
    
end 
