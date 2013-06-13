
functor TypeChecker (H:TypeHelper) =
struct
	open H
	structure S = Syntax (E)
	open S
	structure E2 = LatticeOps.Func (E) (E)
	
	exception TypeError of string
	
	fun emap f (GeoSamps(a1,a2)) = GeoSamps (f a1, f a2)
	  | emap f (Hits a) = Hits (f a)
	  | emap f (Frags a)  = Frags (f a)
	
	fun ezip f _ (GeoSamps(a1,a2)) (GeoSamps(b1,b2)) = GeoSamps (f a1 b1, f a2 b2)
	  | ezip f _ (Hits a) (Hits b) = Hits (f a b)
	  | ezip f _ (Frags a) (Frags b) = Frags (f a b)
	  | ezip f d _ _ = d

	fun etest f (GeoSamps(a1,a2)) (GeoSamps(b1,b2)) = (f a1 b1) andalso (f a2 b2)
	  | etest f (Hits a) (Hits b) = f a b
	  | etest f (Frags a) (Frags b) = f a b
	  | etest f _ _ = false
	
	fun purityLT KeyDecomp KeyDecomp = true
	  | purityLT KeyDecomp GenDecomp = true
	  | purityLT GenDecomp KeyDecomp = false
	  | purityLT GenDecomp GenDecomp = true
	  
	fun typeLT (Decomp (p1, d1)) (Decomp (p2, d2)) = 
		(purityLT p1 p2) andalso (etest E2.lt d1 d2)
	  | typeLT (Arrow (a1, a2)) (Arrow (b1, b2)) = 
		(etest E.lt b1 a1) andalso (etest E.lt a2 b2)
	  | typeLT (Arrow _) (Decomp _) = false
	  | typeLT (Decomp _) (Arrow _) = false
	
	fun getBasicDecompPurity OneG = GenDecomp
	  | getBasicDecompPurity TwoGP = GenDecomp
	  | getBasicDecompPurity OneS = KeyDecomp
	  | getBasicDecompPurity SixteenSqSP = KeyDecomp
	  | getBasicDecompPurity OneH = KeyDecomp
	
	fun annotateTypesH g (EChain (e1,e2)) = 
			let 
				val r1 as Expr (_,t1) = annotateTypes g e1
				val r2 as Expr (_,t2) = annotateTypes g e2
			in
				case (t1,t2) of
				  (Arrow (t_in, t_mid1), Arrow (t_mid2, t_out)) => 
					if etest E.lt t_mid1 t_mid2 
					then (EChain (r1,r2), Arrow (t_in, t_out))
					else raise TypeError "Type mismatch at chain."
				| (_, _) => raise TypeError "Chain requires arrows on both sides."
			end
	  | annotateTypesH g (EMerge (m,e1,e2)) =
			let 
				val r1 as Expr (_,t1) = annotateTypes g e1
				val r2 as Expr (_,t2) = annotateTypes g e2
				val decomp_domain = 
					case (m,t1) of
					  (General, Decomp (GenDecomp, t_d)) => emap #2 t_d
					| (Union, Decomp (KeyDecomp, t_d)) => emap #2 t_d
					| (_, Arrow _) => raise TypeError "Merge requires a decomposition on the left."
					| (_, _) => raise TypeError "Merge method not correct."
				val rhs_input = 
					case t2 of
					 Arrow (t_input, _) => t_input
					| _ => raise TypeError "Merge requires an arrow on the right."
					
			in
				if etest E.lt decomp_domain rhs_input 
				then (EMerge (m,r1,r2), t2)
				else raise TypeError "Type mismatch at merge."
			end
	  | annotateTypesH g (ESizeCase (dom, n, e1, e2)) =
			let 
				val r1 as Expr (_,t1) = annotateTypes g e1
				val r2 as Expr (_,t2) = annotateTypes g e2
				fun matchSizes inLeft inRight = raise Fail "Not implemented"
				(*	if not (E.lt inLeft (SClosed (1,n-1))) then raise TypeError "Left inbound type mismatch at size case." 
					else if not (E.lt inRight (SOpen n)) then raise TypeError "Right inbound type mismatch at size case."  
					else E.join inLeft inRight	*)	
				fun matchCaseInput GeoCase (GeoSamps (gL,sL)) (GeoSamps (gR,sR)) = GeoSamps (matchSizes gL gR, E.join sL sR)
				  | matchCaseInput SampCase (GeoSamps (gL,sL)) (GeoSamps (gR,sR)) = GeoSamps (E.join gL gR, matchSizes sL sR)
				  | matchCaseInput HitCase (Hits inLeft) (Hits inRight) = Hits(matchSizes inLeft inRight)
				  | matchCaseInput FragCase (Frags inLeft) (Frags inRight) = Frags (matchSizes inLeft inRight)
				  | matchCaseInput _ _ _ = raise TypeError "Entity type mismatch at input to size case."  
			in
				case (t1,t2) of
				  (Arrow (t_in1, t_out1), Arrow (t_in2, t_out2)) => 
					if not (t_out1 = t_out2) then raise TypeError "Outbound type mismatch at size case."
					else (ESizeCase (dom, n, r1,r2), Arrow (matchCaseInput dom t_in1 t_in2, t_out1))
				| (_, _) => raise TypeError "Size case requires arrows on both sides."
			end
	  | annotateTypesH g (EFix (label, suggestedType, e)) = 
			let 
				val r as Expr (_,foundType) = annotateTypes ((label,suggestedType)::g) e
			in
				if typeLT foundType suggestedType
				then (EFix (label, suggestedType, r), suggestedType)
				else raise TypeError "Type error at fixed point."
			end
	  | annotateTypesH g (ELabel label) = (
			case List.find (fn x => #1 x = label) g of 
			  SOME (_,t) => (ELabel label,t) 
			| NONE => raise TypeError "Undefined label!")
	  | annotateTypesH _ (EFilt (t,e)) = 
			let 
				val r as Expr (_,foundType) = annotateTypes g e
			in
				(EFilt(t,r), annotateTypes g e)
			end
	  | annotateTypesH _ EHit = (EHit, Arrow(GeoSamps (justOne,justOne), Hits justOne))
	  | annotateTypesH _ EShade = (EShade, Arrow(Hits justOne, Frags justOne))
	  | annotateTypesH _ (EDecomp d) = (EDecomp(d), Decomp (getBasicDecompPurity d, getBasicDecompType d))
	
	and annotateTypes g (Expr (exp, ())) = Expr (annotateTypesH g exp)
end

structure TrivialHelper :> TypeHelper = 
struct
	open BasicSyntax
	structure E = LatticeOps.Trivial
	fun getBasicDecompType OneG = GeoSamps (((),()),((),()))
	  | getBasicDecompType TwoGP = GeoSamps (((),()),((),()))
	  | getBasicDecompType OneS = GeoSamps (((),()),((),()))
	  | getBasicDecompType SixteenSqSP = GeoSamps (((),()),((),()))
	  | getBasicDecompType OneH = Hits ((),())
	val justOne = ()
end 