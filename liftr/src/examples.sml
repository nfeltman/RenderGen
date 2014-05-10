
structure Examples = 
struct

open Lambda12c
open LangCommon
structure Comp = ValueComparison

val _ = Variable.reset ()

fun j test = [test]
fun k (name,prog) = [(name^"-first",prog), (name^"-second","next{"^prog^"}")]
val programs = [
k("fourPlusSix", 		"4 + 6"),
k("twoTimesThree", 		"2 * 3"),
k("twoGtThree", 		"2 > 3"),
k("proj1", 				"#1 (3,true)"),
k("proj2", 				"#2 (3,true)"),
k("proj3", 				"3 + #1 (3,true)"),
k("proj4", 				"#1 (3,true) + 3"),
k("let1", 				"let x = 4 + 6 in x * x"),
k("let2", 				"let (x,b) = (4+6,false) in if b then x else x * x"),
j("letnext",			"let x = next{4 + 6} in next {prev{x} * prev{x}}" ),
j("doubleBind", 		"let x = next{4 + 6} in let x = next{prev{x} * prev{x}} in next{prev{x} * prev{x}}" ),
k("if1", 				"if 3 > 2 then 4 + 6 else 2 * 3"),
j("ifFirstThenSecnod",	"if 3 > 2 then next{4 + 6} else next{2 * 3}"),
j("if2", 				"next{ if 2 > 3 then prev{hold (4 + 6)} else 2 * 3}"),
j("ifBothSides",		"next{ if 2 > 3 then prev{hold (4 + 6)} else prev{hold (2 * 3)}}"),
j("ifPred",				"next{ if 2 > prev{hold (2 + 4)} then prev{hold (4 + 6)} else prev{hold (2 * 3)}}"),
j("if7",				"if 3 > 2 then next{prev{hold (8 * 9)} + 6} else next{prev{hold (8 + 9)} * prev{hold (6 - 2)}}"),
j("holdif",				"hold (if 2 > 3 then 1 else 0)"),
j("holdif2",			"next {prev {hold (if 2 > 3 then 1 else 0)}}"),
k("funcApp1", 			"(fn x : int => x + x) 45"),
k("funcApp2", 			"let f = fn x : int => x + x in f 45"),
k("funcApp3", 			"letfun g (x:int) = x + x in 12 + g 45"),
k("funcApp4", 			"letfun g (x:int) = x + x in g 45 + 12"),
j("multiStageFunc", 	"(fn x : int => next{prev{hold (x * x)} + prev{hold x}}) 45"),
k("caseLeft", 			"case inl int 34 of x => x * x | y => y + y"),
k("caseRight", 			"case inr int 34 of x => x * x | y => y + y"),
k("highOrder1", 		"(fn f:(int->int) => f 5) (fn x:int=>x+x)"),
k("closure", 			"(let x = 3 in fn y:int=> x*y) 5"),
k("higherOrder1", 		"(fn f:(int->int) => fn x:int=> f (f x)) (fn y:int=>y+y) 5"),
j("datastruct2", 		"letfun f (x:int) = next{prev{hold (x*x)}+4} in ((f 1,f 2),(f 3,f 4))"),
j("datastruct3", 		"letfun map (f : int -> $int) = " ^ 
						"fn M:((int*int)*(int*int)) => ((f (#1 (#1 M)), f (#2 (#1 M))), (f (#1 (#2 M)), f (#2 (#2 M)))) in " ^ 
						"map (fn x:int => next{prev{hold (x*x)}+4}) ((1, 2), (3, 4))"),
j("datastruct4", 		"letfun map (f : int -> $int) = " ^ 
						"fn ((M1,M2),(M3,M4)):((int*int)*(int*int)) => ((f M1, f M2), (f M3, f M4)) in " ^ 
						"map (fn x:int => next{prev{hold (x*x)}+4}) ((1, 2), (3, 4))"),
k("roll1",				"roll (int) 5"),
k("roll2",				"roll (int * bool) (234, true)"),
k("unroll1",			"unroll (roll (int * bool) (234, true))"),
k("emptyList",			"let empty = roll (unit + (int * 0)) (inl (int * (mu unit + int * 0)) ()) in empty"),
k("makeList",			"let empty = roll (unit + (int * 0)) (inl (int * (mu unit + int * 0)) ()) in " ^
						"letfun cons (ht : int * mu unit + int * 0) = roll (unit + (int * 0)) (inr unit ht) in "^
						"cons (5, cons (3, empty))"),
k("fact",				"letrec fact (n : int) : int = if n <= 0 then 1 else n * fact (n-1) in fact 5"),
k("sumlist",			"let empty = roll (unit + (int * 0)) (inl (int * (mu unit + int * 0)) ()) in " ^
						"letfun cons (ht : int * mu unit + int * 0) = roll (unit + (int * 0)) (inr unit ht) in "^
						"letrec sum (l : mu unit + int * 0) : int = case unroll l of empty => 0 | (h,t) => h + sum t in "^
						"sum (cons (5, cons (3, empty)))")
]

fun pad s n = concat (s :: List.tabulate (n-(String.size s), fn _ => " "))
				
fun testProgram verbose name p = 
	let
		val _ = Variable.reset ()
		val emit = if verbose >= 1 then print else (fn _ => ())
		val debug = if verbose >= 2 then print else (fn _ => ())
		fun printTerm e = (((PrettyPrinter.printTerm debug) o (PrettyPrinter.resolvePrioTerm 3) o PrintPSF.convertTerm) e; debug "\n")
		
		(* Prologue *)
		val _ = (emit "Starting test: "; emit (pad name 24); emit " ...")
		
		(* Parsing *)
		val parsed = L12Parser.parseString p
		
		(* Stage Propegating *)
		val propegated = PropStage.prop1 parsed
	
		(* Printing Input *)
		val _ = debug "\n\n";
		val _ = printTerm (PrintPSF.convertStage1 propegated);
		val _ = debug "~~~~~~~~~~~\n";
		
		(* Typechecking *)
		val _ = Typecheck12.typeCheck1 Contexts.empty propegated
		
		(* Splitting *)
		val (split1, _, (l,split2)) = StageSplit.coerce1 (StageSplit.stageSplit1 propegated)
		
		(* Printing Split Results *)
		val _ = printTerm (PrintPSF.convertPSF split1);
		val _ = debug "---\n";
		val _ = debug (PrintPSF.pat2string (PrintPSF.convertPSFPattern l));
		val _ = debug ".";
		val _ = printTerm (PrintPSF.convertPSF split2);
		
		(* Erasure Semantics *)
		val valErasure = ErasureSemantics.eval1 Contexts.empty propegated
		val (v1Eras,v2Eras) = Comp.splitErasureValue1 valErasure
				
		(* Diagonal Semantics *)
	(*	val (v1Diag, rDiag) = DiagonalSemantics.eval1 empty propegated
		val _ = (printTerm (PrintPSF.convertDiag rDiag);	print "\n~~~~~~~~~~~\n")
		val v2Diag = DiagonalSemantics.eval2 empty rDiag
		val (v1DiagC, v2DiagC) = (Comp.convertDiagValue1 v1Diag, Comp.convertDiagValue2 v2Diag) *)
		
		(* Evaluating Split Part *)
		val (PSFSemantics.Vtuple [v1Split,pSplit]) = PSFSemantics.evaluate Contexts.empty split1
		val v2Split = PSFSemantics.evaluate (PSFSemantics.extendPattern Contexts.empty l pSplit) split2
		
		
		(* Comparing *)
		fun triComp a b c = (Comp.valueEq a b, Comp.valueEq a c, Comp.valueEq b c)
		val (ed1, es1, ds1) = (*triComp v1Eras v1DiagC v1Split*) triComp v1Eras v1Split v1Split
		val (ed2, es2, ds2) = (*triComp v2Eras v2DiagC v2Split*) triComp v2Eras v2Split v2Split
		val results = [ed1, es1, ds1, ed2, es2, ds2]
		
		(* Epilogue *)
		fun printTestResult b = emit (if b then "P" else "F")
		val _ = if List.all (fn b=>b) results 
				then (debug "\n"; emit "all pass!\n"; debug "\n") 
				else (emit "SOME FAILED: "; List.app printTestResult results; emit "\n")
	in
	(*	printTerm (PrintPSF.convertDiag rDiag);
		print "\n~~~~~~~~~~~\n";*)
		debug "====================\n\n";
		()
	end


fun runtests () = List.app (fn (name,prog) => testProgram 2 name prog) (List.concat programs)
end
