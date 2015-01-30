
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
	  UNIT | BOOL | GT | LT | LTE | GTE | FUN | 
	  REC | FIX | ROLL | UNROLL | TRUE | FALSE | 
	  MU | MOD | DEQ | TYPE | DATA | STR | VAL |
	  PUSH | PUSHA | PUSHP | PUSHS | USCORE |
	  STRLIT of string | MONO
%nonterm EXP of expr | AEXP of expr | BEXP of expr |
	  EXPL of expr list |
	  DTARML of (string * ty option) list | DTARM of string * ty option |
	  MATCH of patt * expr | MATCHL of (patt * expr) list |
	  BINOP of Prims.binops | 
	  TY of ty | ATY of ty | BTY of ty | CTY of ty | 
	  DTY of ty | PRODL of ty list | 
	  PATT of patt | PATTL of patt list

%name L12Parse

%noshift EOF
%keyword LET FUN REC VAL DATA IN CASE OF LPAR RPAR LBRACE RBRACE
%nodefault
%%

(* the parser returns the value associated with the expression *)

	  EXP : BEXP BINOP EXP				(Ebinop(BINOP,BEXP,EXP))
		  | BEXP						(BEXP)

	BINOP : PLUS						(Prims.O2plus)
		  | TIMES						(Prims.O2times)
		  | SUB							(Prims.O2minus)
		  | DIV							(Prims.O2div)
		  | DEQ							(Prims.O2equal)
		  | GT							(Prims.O2greater)
		  | LT							(Prims.O2less)
		  | GTE							(Prims.O2greatereq)
		  | LTE							(Prims.O2lesseq)
		  | MOD							(Prims.O2mod)
		  | CARAT						(Prims.O2cat)
 
	 BEXP : BEXP AEXP					(Eapp (BEXP, AEXP))
		  | PROJ AEXP					(Epi(PROJ - 1,AEXP))
		  | ROLL TY	AEXP				(Eroll (TY, AEXP))
		  | UNROLL AEXP					(Eunroll (AEXP))
		  | HOLD AEXP 					(Ehold(AEXP))
		  | PUSH AEXP					(EpushPrim(AEXP))
		  | PUSHP AEXP					(EpushProd(AEXP))
		  | PUSHS AEXP					(EpushSum(AEXP))
		  | AEXP						(AEXP)

	 AEXP : NUM          												(Eint NUM)
		  | TRUE														(Ebool true)
		  | FALSE														(Ebool false)
		  | STRLIT														(Estr (trim STRLIT))
		  | ID              											(Evar ID)
		  | CASE EXP OF MATCHL											(Ecase(EXP,MATCHL))
		  | LPAR RPAR													(Etuple [])
		  | LPAR EXP RPAR												(EXP) 
		  | LPAR EXP COMMA EXP EXPL RPAR								(Etuple (EXP1 :: EXP2 :: EXPL))
		  | NEXT LBRACE EXP RBRACE										(Enext(EXP))
		  | PREV LBRACE EXP RBRACE										(Eprev(EXP))
		  | MONO LBRACE EXP RBRACE										(Emono(EXP))
		  | LET VAL PATT EQ EXP IN EXP									(Elet(EXP1,(PATT,EXP2)))
		  | FN PATT COLON TY DARROW EXP									(Elam (TY,(PATT,EXP)))
		  | IF EXP THEN EXP ELSE EXP									(Eif(EXP1,EXP2,EXP3))
		  | LET FUN ID LPAR PATT COLON TY RPAR EQ EXP IN EXP			(Elet(Elam(TY,(PATT,EXP1)),(Pvar ID,EXP2)))
		  | LET REC ID LPAR PATT COLON TY RPAR COLON TY EQ EXP IN EXP	(Eletr(ID,TY1,TY2,(PATT,EXP1),EXP2))
		  | LET TYPE ID EQ TY IN EXP									(Eletty (ThisStage,ID,TY,EXP))
		  | LET TYPE DOLLAR ID EQ TY IN EXP								(Eletty (NextStage,ID,TY,EXP))
		  | LET DATA ID EQ DTARML IN EXP								(Eletdata (ThisStage,ID,DTARML,EXP))
		  | LET DATA DOLLAR ID EQ DTARML IN EXP							(Eletdata (NextStage,ID,DTARML,EXP))
		  | LET DATA CARAT ID EQ DTARML IN EXP							(Eletdata (MonoStage,ID,DTARML,EXP))

	 EXPL : 							([])
		  | COMMA EXP EXPL				(EXP :: EXPL)

   DTARML : DTARM						([DTARM])
		  | DTARM BAR DTARML			(DTARM::DTARML)
	DTARM : ID OF TY					((ID, SOME TY))
		  | ID							((ID, NONE))
		  
   MATCHL : MATCH						([MATCH])
		  | MATCH BAR MATCHL			(MATCH :: MATCHL)
	MATCH : PATT DARROW EXP				(PATT, EXP)

	 PATT : ID									(Pvar (ID))
	 	  | USCORE								(Punused)
		  | LPAR PATT COMMA PATT PATTL RPAR 	(Ptuple (PATT1 :: PATT2 :: PATTL))
		  | MONO LBRACE PATT RBRACE 			(Pmono (PATT))
		  | NEXT LBRACE PATT RBRACE 			(Pnext (PATT))

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
		  | CARAT ATY					(Tnow ATY)
		  | LPAR TY RPAR				(TY)
	  