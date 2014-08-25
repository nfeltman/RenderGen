
open Lambda12c
open LangCommon

%%

%eop EOF SEMI

(* %pos declares the type of positions for terminals.
   Each symbol has an associated left and right position. *)

%pos int

%left SUB PLUS
%left TIMES DIV

%term ID of string | NUM of int | PLUS | TIMES | 
      SEMI | EOF | CARAT | DIV | SUB | LBRACE | 
	  RBRACE | LPAR | RPAR | PROJ of int | LET |
	  IN | IF | THEN | ELSE | NEXT | PREV | INL | 
	  INR | CASE | OF | HOLD | EQ | COMMA | FN |
	  DARROW | ARROW | BAR | INT | COLON | DOLLAR |
	  UNIT | BOOL | GT | LT | LTE | GTE | LETF | 
	  LETR | FIX | ROLL | UNROLL | TRUE | FALSE | 
	  MU | MOD | DEQ | LETT1 | LETT2 | LETDT
%nonterm EXP of expr | AEXP of expr | BEXP of expr |
	  EXPL of expr list |
	  DTARML of (string * ty) list |
	  MATCH of patt * expr | MATCHL of (patt * expr) list |
	  BINOP of Prims.binops | 
	  TY of ty | ATY of ty | BTY of ty | CTY of ty | 
	  DTY of ty | PRODL of ty list | 
	  PATT of patt | PATTL of patt list

%name L12Parse

%noshift EOF
%nodefault
%verbose
%%

(* the parser returns the value associated with the expression *)

	  EXP : BEXP BINOP EXP				(Ebinop(BINOP,BEXP,EXP))
		  | BEXP						(BEXP)

	BINOP : PLUS						(Prims.Iplus)
		  | TIMES						(Prims.Itimes)
		  | SUB							(Prims.Iminus)
		  | DIV							(Prims.Idiv)
		  | DEQ							(Prims.Iequal)
		  | GT							(Prims.Igreater)
		  | LT							(Prims.Iless)
		  | GTE							(Prims.Igreatereq)
		  | LTE							(Prims.Ilesseq)
		  | MOD							(Prims.Imod)
 
	 BEXP : BEXP AEXP					(Eapp (BEXP, AEXP))
		  | PROJ AEXP					(Epi(PROJ - 1,AEXP))
		  | INL TY AEXP					(Einjl(TY,AEXP))
		  | INR TY AEXP					(Einjr(TY,AEXP))
		  | ROLL TY	AEXP				(Eroll (TY, AEXP))
		  | UNROLL AEXP					(Eunroll (AEXP))
		  | HOLD AEXP 					(Ehold(AEXP))
		  | AEXP						(AEXP)

	 AEXP : NUM          												(Eint NUM)
		  | TRUE														(Ebool true)
		  | FALSE														(Ebool false)
		  | ID              											(Evar ID)
		  | CASE EXP OF MATCHL											(Ecase(EXP,MATCHL))
		  | LPAR RPAR													(Etuple [])
		  | LPAR EXP RPAR												(EXP) 
		  | LPAR EXP COMMA EXP EXPL RPAR								(Etuple (EXP1 :: EXP2 :: EXPL))
		  | NEXT LBRACE EXP RBRACE										(Enext(EXP))
		  | PREV LBRACE EXP RBRACE										(Eprev(EXP))
		  | LET PATT EQ EXP IN EXP										(Elet(EXP1,(PATT,EXP2)))
		  | FN PATT COLON TY DARROW EXP									(Elam (TY,(PATT,EXP)))
		  | IF EXP THEN EXP ELSE EXP									(Eif(EXP1,EXP2,EXP3))
		  | LETF ID LPAR PATT COLON TY RPAR EQ EXP IN EXP				(Elet(Elam(TY,(PATT,EXP1)),(Pvar ID,EXP2)))
		  | LETR ID LPAR PATT COLON TY RPAR COLON TY EQ EXP IN EXP		(Eletr(ID,TY1,TY2,(PATT,EXP1),EXP2))
		  | LETT1 ID EQ TY IN EXP										(Eletty (StageOne,ID,TY,EXP))
		  | LETT2 ID EQ TY IN EXP										(Eletty (StageTwo,ID,TY,EXP))
		  | LETDT ID EQ DTARML IN EXP									(Eletdata (ID1,DTARML,EXP))

	 EXPL : 							([])
		  | COMMA EXP EXPL				(EXP :: EXPL)

   DTARML : ID OF TY					([(ID,TY)])
		  | ID OF TY BAR DTARML			((ID,TY)::DTARML)
		  
   MATCHL : MATCH						([MATCH])
		  | MATCH BAR MATCHL			(MATCH :: MATCHL)
	MATCH : PATT DARROW EXP				(PATT, EXP)

	 PATT : ID									(Pvar (ID))
		  | LPAR PATT COMMA PATT PATTL RPAR 	(Ptuple (PATT1 :: PATT2 :: PATTL))

	PATTL : 							([])
		  | COMMA PATT PATTL			(PATT :: PATTL)

	   TY : DTY ARROW TY				(Tarr(DTY,TY))
		  | DTY 						(DTY)
	  DTY : MU DTY						(Trec DTY)
		  | CTY							(CTY)
	  CTY : BTY PLUS BTY				(Tsum(BTY1,BTY2))
		  | BTY							(BTY)
	   
	  BTY : ATY TIMES ATY PRODL			(Tprod (ATY1 :: ATY2 :: PRODL))
		  | ATY							(ATY)
	PRODL : 							([])
		  | TIMES ATY PRODL				(ATY :: PRODL)
	  
	  ATY : INT							(Tint)
		  | BOOL						(Tbool)
		  | UNIT						(Tprod [])
		  | NUM							(Tvar NUM)
		  | ID							(Tref ID)
		  | DOLLAR ATY					(Tfut ATY)
		  | LPAR TY RPAR				(TY)
	  