
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
	  RBRACE | LPAR | RPAR | PROJL | PROJR | LET |
	  IN | IF | THEN | ELSE | NEXT | PREV | INL | 
	  INR | CASE | OF | HOLD | EQ | COMMA | FN |
	  DARROW | ARROW | BAR | INT | COLON | DOLLAR |
	  UNIT | BOOL | GT | LT | LTE | GTE | LETF | 
	  LETR | FIX | ROLL | UNROLL | TRUE | FALSE | 
	  MU | MOD | DEQ
%nonterm EXP of expr | MATCH of patt * expr |
	  TY of ty | AEXP of expr | BEXP of expr |
	  BINOP of Prims.binops | PATT of patt

%name L12Parse

%noshift EOF
%nodefault
%verbose
%%

(* the parser returns the value associated with the expression *)

  EXP : BEXP BINOP EXP				(Ebinop(BINOP,BEXP,EXP))
	  | BEXP						(BEXP)
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
	  | PROJL AEXP					(Epi(Left,AEXP))
	  | PROJR AEXP					(Epi(Right,AEXP))
	  | INL TY AEXP					(Einj(Left,TY,AEXP))
	  | INR TY AEXP					(Einj(Right,TY,AEXP))
	  | ROLL TY	AEXP				(Eroll (TY, AEXP))
	  | UNROLL AEXP					(Eunroll (AEXP))
	  | HOLD AEXP 					(Ehold(AEXP))
	  | AEXP						(AEXP)

 AEXP : NUM          												(Eint NUM)
	  | TRUE														(Ebool true)
	  | FALSE														(Ebool false)
      | ID              											(Evar ID)
	  | CASE EXP OF MATCH BAR MATCH									(Ecase(EXP,MATCH1,MATCH2))
	  | LPAR RPAR													(Eunit)
	  | LPAR EXP COMMA EXP RPAR										(Etuple(EXP1,EXP2))
	  | NEXT LBRACE EXP RBRACE										(Enext(EXP))
	  | PREV LBRACE EXP RBRACE										(Eprev(EXP))
	  | LPAR EXP RPAR												(EXP) 
	  | LET PATT EQ EXP IN EXP										(Elet(EXP1,(PATT,EXP2)))
	  | FN PATT COLON TY DARROW EXP									(Elam (TY,(PATT,EXP)))
	  | IF EXP THEN EXP ELSE EXP									(Eif(EXP1,EXP2,EXP3))
	  | LETF ID LPAR PATT COLON TY RPAR EQ EXP IN EXP				(Elet(Elam(TY,(PATT,EXP1)),(Pvar ID,EXP2)))
	  | LETR ID LPAR PATT COLON TY RPAR COLON TY EQ EXP IN EXP		(Eletr(ID,TY1,TY2,(PATT,EXP1),EXP2))
	  
MATCH : PATT DARROW EXP				(PATT, EXP)

 PATT : ID							(Pvar (ID))
	  | LPAR PATT COMMA PATT RPAR 	(Ptuple (PATT1, PATT2))

   TY : INT							(Tint)
      | BOOL						(Tbool)
      | UNIT						(Tunit)
	  | NUM							(Tvar NUM)
	  | MU TY						(Trec TY)
      | DOLLAR TY					(Tfut TY)
      | TY TIMES TY					(Tprod(TY1,TY2))
      | TY PLUS TY					(Tsum(TY1,TY2))
	  | TY ARROW TY					(Tarr(TY1,TY2))
	  | LPAR TY RPAR				(TY)