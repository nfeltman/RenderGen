
structure Examples = 
struct

open Lambda12c
open LangCommon

val _ = Variable.reset ()
val x = Variable.newvar "x"
val fourPlusSix = Ebinop(Prims.Iplus, Eint 4, Eint 6)
val twoTimesThree = Ebinop(Prims.Itimes, Eint 2, Eint 3)
val twoGtThree = Ebinop(Prims.Igreater, Eint 2, Eint 3)
val letAll1 = Elet(fourPlusSix, (x, Ebinop(Prims.Itimes, Evar x, Evar x)))
val let12 = Elet(Enext fourPlusSix, (x, Enext (Ebinop(Prims.Itimes, Eprev(Evar x), Eprev(Evar x)))))
val doubleBind = Elet(Enext fourPlusSix, 
				(x, Elet (Enext (Ebinop(Prims.Itimes, Eprev(Evar x), Eprev(Evar x))), 
							(x, Enext (Ebinop(Prims.Itimes, Eprev(Evar x), Eprev(Evar x)))))))
val if1 = Eif(Ebool true, Enext fourPlusSix, Ehold twoTimesThree)
val if2 = Enext( Eif(twoGtThree, Eprev (Ehold fourPlusSix), twoTimesThree))

fun pad s n = concat (s :: List.tabulate (n-(String.size s), fn _ => " "))
				
fun testProgram verbose name p = 
	let
		val emit = if verbose >= 1 then print else (fn _ => ())
		val debug = if verbose >= 2 then print else (fn _ => ())
		
		(* Prologue *)
		val _ = (emit "Starting test: "; emit (pad name 24); emit " ...")
		
		(* Checking Input *)
		val propegated = PropStage.prop1 p
		val _ = Typecheck12.typeCheck1 empty propegated
		
		(* Erasure Semantics *)
		val valErasure = ErasureSemantics.eval1 empty propegated
		val (v1Eras,v2Eras) = ErasureSemantics.splitValue1 valErasure
		
		(* Diagonal Semantics *)
		val (v1Diag, rDiag) = DiagonalSemantics.eval1 empty propegated
		val v2Diag = DiagonalSemantics.eval2 empty rDiag
		
		(* Splitting *)
		val (split1, _, (l,split2), _) = StageSplit.stageSplit1 empty propegated
		val (PSFSemantics.Vtuple [v1Split,pSplit]) = PSFSemantics.evaluate empty split1
		val v2Split = PSFSemantics.evaluate (extendContext empty l pSplit) split2
		
		(* Epilogue *)
		val _ = (emit "all pass!\n")
	in
		()
	end


fun runtests () = 

	let
		val testProgram = testProgram 1
		val _ = testProgram "fourPlusSix" fourPlusSix
		val _ = testProgram "twoTimesThree" twoTimesThree
		val _ = testProgram "twoGtThree" twoGtThree
		val _ = testProgram "letAll1" letAll1
		val _ = testProgram "doubleBind" doubleBind
		val _ = testProgram "let12" let12
		val _ = testProgram "if1" if1
		val _ = testProgram "if2" if2
	in
		()
	end
end
