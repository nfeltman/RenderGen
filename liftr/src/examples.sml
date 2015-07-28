
structure Examples = 
struct

open SourceLang
open Lambda12c
open LangCommon
open ErasureSemantics
structure Comp = ValueComparison

datatype progSource = Literal | FileName
datatype testLevel = NONE | SAME | EXACT of value
val ansI = EXACT o V1mono o V1 o ValuesBase.VFprim o Prims.Vint
val ansNI = EXACT o V1next o V1 o ValuesBase.VFprim o Prims.Vint
val ansB = EXACT o V1mono o V1 o ValuesBase.VFprim o Prims.Vbool
val ansNB = EXACT o V1next o V1 o ValuesBase.VFprim o Prims.Vbool
val ansS = EXACT o V1mono o V1 o ValuesBase.VFprim o Prims.Vstr

infixr 9 `
fun a ` b = a b

val _ = Variable.reset ()

fun i (name,filename,t) = [(name,FileName,filename,t)]
fun j (name,prog,t) = [(name,Literal,prog,t)]
fun k (name,prog,t) = [
		(name^"-first",Literal,prog,t), 
		(name^"-second",Literal,"next{"^prog^"}", case t of EXACT v => EXACT (holdGeneral v) | _ => t)]
val programs = [
k("stringlit", 			"\"fourplussix\"",SAME),
k("concat", 			"\"four\" ^ \"six\"",ansS "foursix"),
k("fourPlusSix", 		"4 + 6",ansI 10),
k("twoTimesThree", 		"2 * 3",ansI 6),
k("twoGtThree", 		"2 > 3",ansB false),
k("proj1", 				"#1 (3,true)",ansI 3),
k("proj2", 				"#2 (3,true)",ansB true),
k("proj3", 				"3 + #1 (3,true)",ansI 6),
k("proj4", 				"#1 (3,true) + 3",ansI 6),
k("let1", 				"let val x = 4 + 6 in x * x",ansI 100),
k("let2", 				"let val (x,b) = (4+6,false) in if b then x else x * x",ansI 100),
j("letnext",			"let val x = next{4 + 6} in next {prev{x} * prev{x}}",SAME),
j("monopatt",			"let val mono{(x,y)} = mono{(1,2)} in mono {x+y}",SAME),
j("nextpatt",			"let val next{(x,y)} = next{(1,2)} in next {x+y}",SAME),
j("doubleBind", 		"let val x = next{4 + 6} in let val x = next{prev{x} * prev{x}} in next{prev{x} * prev{x}}",SAME),
k("if1", 				"if 3 > 2 then 4 + 6 else 2 * 3",ansI 10),
j("ifFirstThenSecnod",	"if 3 > 2 then next{4 + 6} else next{2 * 3}",SAME),
j("if2", 				"next{ if 2 > 3 then prev{hold mono{4 + 6}} else 2 * 3}",SAME),
j("ifBothSides",		"next{ if 2 > 3 then prev{hold mono{4 + 6}} else prev{hold mono{2 * 3}}}",SAME),
j("ifPred",				"next{ if 2 > prev{hold mono{2 + 4}} then prev{hold mono{4 + 6}} else prev{hold mono{2 * 3}}}",SAME),
j("if7",				"if 3 > 2 then next{prev{hold mono{8 * 9}} + 6} else next{prev{hold mono{8 + 9}} * prev{hold mono{6 - 2}}}",SAME),
j("holdif",				"hold mono{if 2 > 3 then 1 else 0}",SAME),
j("holdif2",			"next {prev {hold mono{if 2 > 3 then 1 else 0}}}",SAME),
k("funcApp1", 			"(fn x : int => x + x) 45",ansI 90),
k("funcApp2", 			"let val f = fn x : int => x + x in f 45",ansI 90),
k("funcApp3", 			"let fun g (x:int) = x + x in 12 + g 45",ansI 102),
k("funcApp4", 			"let fun g (x:int) = x + x in g 45 + 12",ansI 102),
j("multiStageFunc", 	"(fn mono{x} : ^int => next{prev{hold mono{x * x}} + prev{hold mono{x}}}) mono{45}",SAME),
k("datatype3", 			"let datatype t = A of int | B of int * int | C of int * int * int in " ^
						"case unroll (B (3,4)) of x => x | (x,y) => x+y | (x,y,z) => x+y+z",ansI 7),
k("highOrder1", 		"(fn f:int->int => f 5) (fn x:int=>x+x)",ansI 10),
k("closure", 			"(let val x = 3 in fn y:int=> x*y) 5",ansI 15),
k("higherOrder1", 		"(fn f:int->int => fn x:int=> f (f x)) (fn y:int=>y+y) 5",ansI 20),
j("datastruct2", 		"let fun f (mono{x}:^int) = next{prev{hold mono{x*x}}+4} in ((f mono{1},f mono{2}),(f mono{3},f mono{4}))",SAME),
j("datastruct3", 		"let fun map (f : ^int -> $int) = " ^ 
						"fn M:((^int*^int)*(^int*^int)) => ((f (#1 (#1 M)), f (#2 (#1 M))), (f (#1 (#2 M)), f (#2 (#2 M)))) in " ^ 
						"map (fn mono{x}:^int => next{prev{hold mono{x*x}}+4}) ((mono{1}, mono{2}), (mono{3}, mono{4}))",SAME),
j("datastruct4", 		"let fun map (f : ^int -> $int) = " ^ 
						"fn ((M1,M2),(M3,M4)):((^int*^int)*(^int*^int)) => ((f M1, f M2), (f M3, f M4)) in " ^ 
						"map (fn mono{x}:^int => next{prev{hold mono{x*x}}+4}) ((mono{1}, mono{2}), (mono{3}, mono{4}))",SAME),
j("datastruct5", 		"let fun map (f : ^int -> $int) = " ^ 
						"fn (M1,M2,M3,M4):(^int*^int*^int*^int) => (f M1, f M2, f M3, f M4) in " ^ 
						"map (fn mono{x}:^int => next{prev{hold mono{x*x}}+4}) (mono{1}, mono{2}, mono{3}, mono{4})",SAME),
k("roll1",				"roll (int) 5",SAME),
k("roll2",				"roll (int * bool) (234, true)",SAME),
k("unroll1",			"#1 (unroll (roll (int * bool) (234, true)))",ansI 234),
k("fact",				"let rec fact (n : int) : int = if n <= 0 then 1 else n * fact (n-1) in fact 5",ansI 120),
k("sumlist",			"let datatype list = Empty | Cons of int * list in " ^
						"let rec sum (l : list) : int = case unroll l of empty => 0 | (h,t) => h + sum t in "^
						"sum (Cons (5, Cons (3, Empty)))",ansI 8),
i("prefixtree",			"prefixtree", ansNB true),
i("iota",				"iota", ansI 5),
i("fastexp",			"fastexp",ansNI 243),
i("vectors",			"vectors", ansNI 32),
i("quickselect_fixed",	"quickselect_fixed", ansNI 4),
i("quickselect",		"quickselect", ansNI 4),
i("stress",				"stress", SAME)
]

fun pad s n = concat (s :: List.tabulate (n-(String.size s), fn _ => " "))

exception Problem				
fun testProgram verbose name programType p t = 
	let
		val _ = Variable.reset ()
		val emit = if verbose >= 1 then print else (fn _ => ())
		val debug = if verbose >= 2 then print else (fn _ => ())
	(*	fun printTerm e = (((PrettyPrinter.printTerm debug) o (PrettyPrinter.resolvePrioTerm 3) o PrintPSF.convertTerm) e; debug "\n") *)
		fun printTerm e = (PrettyPrinter.printExp debug e; debug "\n")
		
		(* Prologue *)
		val _ = (emit "Starting test: "; emit (pad name 24); emit " ...")
		
		(* Parsing *)
		val parsed = 
			case programType of 
			  Literal => L12Parser.parseString p
			| FileName => L12Parser.parseFile ("examples/"^p^".L12")
		
		(* Stage Propegating *)
		val propegated = PropStage.prop parsed
			handle PropStage.Cont.UnboundVar s => (emit "Unbound Variable: "; emit s; raise Problem)
	
		(* Printing Input *)
		val _ = debug "\n\n";
		val _ = printTerm (PrintPSF.convertL12 propegated);
		val _ = debug "~~~~~~~~~~~\n";
		
		(* Typechecking *)
		val ty1 = Typecheck12.typeCheck1 Typecheck12.emptyContext propegated
			handle TypeError s => (emit "Type Error: "; emit s; raise Problem)
			handle Typecheck12.MyContext.UnboundVar s => (emit "Unbound Variable: "; emit (Variable.toString s); raise Problem)
		
		(* Splitting *)
		val (ty2, res) = StageSplit.stageSplit1 Typecheck12.emptyContext propegated
			handle TypeError s => (emit "Type Error: "; emit s; raise Problem)
			handle Typecheck12.MyContext.UnboundVar s => (emit "Unbound Variable: "; emit (Variable.toString s); raise Problem)
		val (split1, (l,split2)) = StageSplit.coerce1 res
		
		(* Printing Split Results *)
		val _ = printTerm (PrintPSF.convertPSF split1);
		val _ = debug "---\n";
		val (printerL, printerSplit2) = PrintPSF.convertPSFBranch (l,split2)
		val _ = debug (PrintPSF.pat2string printerL);
		val _ = debug ".\n";
		val _ = printTerm printerSplit2;
		val _ = debug "~~~~~~~~~~~\n";
		
		(* Erasure Semantics *)
		val _ = debug "Running erasure semantics.\n";
		val valErasure = ErasureSemantics.eval MainDict.empty propegated
				
		(* Diagonal Semantics *)
		val _ = debug "Running diagonal semantics, part 1.\n"
		val (xiDiag, v1Diag) = DiagonalSemantics.eval1 MainDict.empty propegated
		val (v1DiagC, diagBody) = Comp.splitDiagValue1 v1Diag
		val diagResidual = xiDiag diagBody
		val _ = debug "Running diagonal semantics, part 2.\n";
		val v2Diag = DiagonalSemantics.eval2 DiagonalSemantics.Context2.empty diagResidual
		val v2DiagC = Comp.convertDiagValue2 v2Diag
		
		(* Printing Diagonal Semantics *)
(*		val _ = printTerm ` PrintPSF.convertDiag diagResidual
		val _ = debug "~~~~~~~~~~~\n"; *)
		
		(* Evaluating Split Part *)
		val _ = debug "Running split term, part 1.\n"
		val (PSFSemantics.V (ValuesBase.VFtuple [v1Split,pSplit])) = PSFSemantics.evaluate PSFSemantics.Context.empty split1
		val _ = debug "Value = \n"
		val _ = printTerm (PrintPSF.convertPSFVal pSplit)
		val _ = debug "Running split term, part 2.\n"
		val v2Split = PSFSemantics.evaluate (PSFSemantics.extendPattern PSFSemantics.Context.empty l pSplit) split2
		
		
		(* Comparing *)
		fun triComp a b c = (Comp.valueEq a b, Comp.valueEq a c, Comp.valueEq b c)
		fun triCompAgainst v a b c = (Comp.valueEq v a, Comp.valueEq v b, Comp.valueEq v c)
		val results = 
		case t of
		  NONE => []
		| SAME => let
			val (v1Eras,v2Eras) = Comp.splitErasureValue1 valErasure
			val (ed1, es1, ds1) = triComp v1Eras v1DiagC v1Split
			val (ed2, es2, ds2) = triComp v2Eras v2DiagC v2Split
			in [ed1, es1, ds1, ed2, es2, ds2] end
		| EXACT v => let
			val (v1Eras,v2Eras) = Comp.splitErasureValue1 valErasure
			val (v1Suplied,v2Suplied) = Comp.splitErasureValue1 v
			val (ae1, ad1, as1) = triCompAgainst v1Suplied v1Eras v1DiagC v1Split
			val (ae2, ad2, as2) = triCompAgainst v2Suplied v2Eras v2DiagC v2Split
			in [ae1, ad1, as1, ae2, ad2, as2] end
		
		(* Epilogue *)
		fun printTestResult b = emit (if b then "P" else "F")
		val _ = if List.all (fn b=>b) results 
				then (debug "\n"; emit (case results of [] => "no tests!\n" | _ => "all pass!\n"); debug "\n") 
				else (emit "SOME FAILED: "; List.app printTestResult results; emit "\n"; raise Problem)
	in
	(*	printTerm (PrintPSF.convertDiag rDiag);
		print "\n~~~~~~~~~~~\n";*)
		debug "====================\n\n";
		()
	end


fun runtests v = List.app (fn (name,progType,prog,t) => testProgram v name progType prog t) (List.concat programs)

(* 
val compiled = CM.make "sources.cm"; if compiled then Examples.runtests 1 else ();
*)
end
