
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
	  UNIT | BOOL | GT
%nonterm EXP of expr | MATCH of string * expr |
	  TY of ty | AEXP of expr

%name L12Parse

%noshift EOF
%nodefault
%verbose
%%

(* the parser returns the value associated with the expression *)

  EXP : AEXP PLUS EXP				(Ebinop(Prims.Iplus,AEXP,EXP))
      | AEXP TIMES EXP   			(Ebinop(Prims.Itimes,AEXP,EXP))
      | AEXP SUB EXP     			(Ebinop(Prims.Iminus,AEXP,EXP))
      | AEXP GT EXP     			(Ebinop(Prims.Igreater,AEXP,EXP))
	  | AEXP CARAT EXP				(Eapp (AEXP, EXP))
	  | AEXP						(AEXP)

 AEXP : NUM          				(Eint NUM)
      | ID              			(Evar ID)
	  | FN ID COLON TY DARROW EXP	(Elam (TY,(ID,EXP)))
	  | IF EXP THEN EXP ELSE EXP	(Eif(EXP1,EXP2,EXP3))
	  | CASE EXP OF MATCH BAR MATCH	(Ecase(EXP,MATCH1,MATCH2))
	  | PROJL EXP					(Epi(Left,EXP))
	  | PROJR EXP					(Epi(Right,EXP))
	  | INL TY EXP					(Einj(Left,TY,EXP))
	  | INR TY EXP					(Einj(Right,TY,EXP))
	  | LPAR RPAR					(Eunit)
	  | LPAR EXP COMMA EXP RPAR		(Etuple(EXP1,EXP2))
	  | NEXT LBRACE EXP RBRACE		(Enext(EXP))
	  | PREV LBRACE EXP RBRACE		(Eprev(EXP))
	  | HOLD EXP 					(Ehold(EXP))
	  | LET ID EQ EXP IN EXP		(Elet(EXP1,(ID,EXP2)))
	  | LPAR EXP RPAR				(EXP) 
	  
MATCH : ID DARROW EXP				((ID, EXP))

   TY : INT							(Tint)
      | BOOL						(Tbool)
      | UNIT						(Tunit)
      | DOLLAR TY					(Tfut TY)
      | TY TIMES TY					(Tprod(TY1,TY2))
      | TY PLUS TY					(Tsum(TY1,TY2))
	  | LPAR TY RPAR				(TY)