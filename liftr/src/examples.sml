
structure Examples = 
struct

open Lambda12c
open LangCommon
structure Comp = ValueComparison

val _ = Variable.reset ()

val programs = [
("fourPlusSix", 		"4 + 6"),
("twoTimesThree", 		"2 * 3"),
("twoGtThree", 			"2 > 3"),
("letAll1", 			"let x = 4 + 6 in x * x"),
("let12", 				"let x = next{4 + 6} in next {prev{x} * prev{x}}" ),
("doubleBind", 			"let x = next{4 + 6} in let x = next{prev{x} * prev{x}} in next{prev{x} * prev{x}}" ),
("if1", 				"if 3 > 2 then 4 + 6 else 2 * 3"),
("ifFirstThenSecnod",	"if 3 > 2 then next{4 + 6} else next{2 * 3}"),
("if2", 				"next{ if 2 > 3 then prev{hold (4 + 6)} else 2 * 3}"),
("ifBothSides",			"next{ if 2 > 3 then prev{hold (4 + 6)} else prev{hold (2 * 3)}}"),
("ifPred",				"next{ if 2 > prev{hold (2 + 4)} then prev{hold (4 + 6)} else prev{hold (2 * 3)}}"),
("if7",					"if 3 > 2 then next{prev{hold (8 * 9)} + 6} else next{prev{hold (8 + 9)} * prev{hold (6 - 2)}}"),
("holdif",				"hold (if 2 > 3 then 1 else 0)"),
("holdif2",				"next {prev {hold (if 2 > 3 then 1 else 0)}}"),
("funcApp1", 			"(fn x : int => x + x) 45"),
("funcApp2", 			"let f = fn x : int => x + x in f 45"),
("funcApp3", 			"letfun g (x:int) = x + x in 12 + g 45"),
("multiStageFunc", 		"(fn x : int => next{prev{hold (x * x)} + prev{hold x}}) 45"),
("caseLeft", 			"case inl int 34 of x => x * x | y => y + y"),
("caseRight", 			"case inr int 34 of x => x * x | y => y + y"),
("highOrder1", 			"(fn f:(int->int)=> f 5) (fn x:int=>x+x)"),
("closure", 			"(let x = 3 in fn y:int=> x*y) 5"),
("higherOrder1", 		"(fn f:(int->int)=> fn x:int=> f (f x)) (fn y:int=>y+y) 5"),
(* map a multi-stage function over a datastructure; inline / bind func / higher order map *)
("datastruct2", 		"letfun f (x:int) = next{prev{hold (x*x)}+4} in ((f 1,f 2),(f 3,f 4))"),
("datastruct3", 		"letfun map (f : int -> $int) = fn M:((int*int)*(int*int)) => ((f (#1 (#1 M)), f (#2 (#1 M))), (f (#1 (#2 M)), f (#2 (#2 M)))) in map (fn x:int => next{prev{hold (x*x)}+4}) ((1, 2), (3, 4))")
]

fun pad s n = concat (s :: List.tabulate (n-(String.size s), fn _ => " "))
				
fun testProgram verbose name p = 
	let
		val printTerm = (PrettyPrinter.printTerm print) o (PrettyPrinter.resolvePrioTerm 3) o PrintPSF.convertTerm
		val _ = Variable.reset ()
		val emit = if verbose >= 1 then print else (fn _ => ())
		val debug = if verbose >= 2 then print else (fn _ => ())
		
		(* Prologue *)
		val _ = (emit "Starting test: "; emit (pad name 24); emit " ...")
		
		(* Parsing *)
		val parsed = L12Parser.parseString p
		
		(* Checking Input *)
		val propegated = PropStage.prop1 parsed
		val _ = Typecheck12.typeCheck1 empty propegated
		
		
		(* Erasure Semantics *)
		val valErasure = ErasureSemantics.eval1 empty propegated
		val (v1Eras,v2Eras) = Comp.splitErasureValue1 valErasure
				
		(* Diagonal Semantics *)
	(*	val (v1Diag, rDiag) = DiagonalSemantics.eval1 empty propegated
		val _ = (printTerm (PrintPSF.convertDiag rDiag);	print "\n~~~~~~~~~~~\n")
		val v2Diag = DiagonalSemantics.eval2 empty rDiag
		val (v1DiagC, v2DiagC) = (Comp.convertDiagValue1 v1Diag, Comp.convertDiagValue2 v2Diag) *)
		
		(* Splitting *)
		val (split1, _, (l,split2)) = StageSplit.coerce1 (StageSplit.stageSplit1 propegated)
		val (PSFSemantics.Vtuple [v1Split,pSplit]) = PSFSemantics.evaluate empty split1
		val v2Split = PSFSemantics.evaluate (extendContext empty l pSplit) split2
		
		(* Comparing *)
		fun triComp a b c = (Comp.valueEq a b, Comp.valueEq a c, Comp.valueEq b c)
		val (ed1, es1, ds1) = (*triComp v1Eras v1DiagC v1Split*) triComp v1Eras v1Split v1Split
		val (ed2, es2, ds2) = (*triComp v2Eras v2DiagC v2Split*) triComp v2Eras v2Split v2Split
		val results = [ed1, es1, ds1, ed2, es2, ds2]
		
		(* Epilogue *)
		fun printTestResult b = emit (if b then "P" else "F")
		val _ = if List.all (fn b=>b) results 
				then (emit "all pass!\n") 
				else (emit "SOME FAILED: "; List.app printTestResult results; emit "\n")
	in
	
		print "\n\n";
		printTerm (PrintPSF.convertStage1 propegated);
		print "\n~~~~~~~~~~~\n";
	(*	printTerm (PrintPSF.convertDiag rDiag);
		print "\n~~~~~~~~~~~\n";*)
		printTerm (PrintPSF.convertPSF split1);
		print "\n---\n";
		print (Variable.toString l);
		print ".";
		printTerm (PrintPSF.convertPSF split2);
		print "\n\n====================\n\n";
	
		()
	end


fun runtests () = List.app (fn (name,prog) => testProgram 1 name prog) programs 
end
