structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
val error = fn (e,l : int,_) =>
              print("line " ^ (Int.toString l) ^
                               ": " ^ e ^ "\n")
%%
%header (functor L12LexFun(structure Tokens: L12Parse_TOKENS));
%s L12 CMT STR;
digit=[0-9];
idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
ws = [\ \t];
eol = ("\013\010"|"\010"|"\013");
%%
<INITIAL>{ws}* => (pos:= 0; YYBEGIN L12; lex());
<L12>\n       => (pos := (!pos) + 1; lex());
<L12>{ws}+    => (lex());
<L12>{digit}+ => (Tokens.NUM
                (List.foldl (fn (a,r) => ord(a)-ord(#"0")+10*r)
                         0 (explode yytext),
                  !pos,!pos));
				  
<L12>"%%"     => (YYBEGIN CMT; continue ());
<L12>"+"      => (Tokens.PLUS(!pos,!pos));
<L12>"*"      => (Tokens.TIMES(!pos,!pos));
<L12>":"      => (Tokens.COLON(!pos,!pos));
<L12>";"      => (Tokens.SEMI(!pos,!pos));
<L12>","      => (Tokens.COMMA(!pos,!pos));
<L12>"$"      => (Tokens.DOLLAR(!pos,!pos));
<L12>"="      => (Tokens.EQ(!pos,!pos));
<L12>"=="     => (Tokens.DEQ(!pos,!pos));
<L12>"->"     => (Tokens.ARROW(!pos,!pos));
<L12>"=>"     => (Tokens.DARROW(!pos,!pos));
<L12>">"      => (Tokens.GT(!pos,!pos));
<L12>">="     => (Tokens.GTE(!pos,!pos));
<L12>"<"      => (Tokens.LT(!pos,!pos));
<L12>"<="     => (Tokens.LTE(!pos,!pos));
<L12>"#1"     => (Tokens.PROJ(1,!pos,!pos));
<L12>"#2"     => (Tokens.PROJ(2,!pos,!pos));
<L12>"#3"     => (Tokens.PROJ(3,!pos,!pos));
<L12>"#4"     => (Tokens.PROJ(4,!pos,!pos));
<L12>"#5"     => (Tokens.PROJ(5,!pos,!pos));
<L12>"#6"     => (Tokens.PROJ(6,!pos,!pos));
<L12>"#7"     => (Tokens.PROJ(7,!pos,!pos));
<L12>"#8"     => (Tokens.PROJ(8,!pos,!pos));
<L12>{id}     => (case yytext of
				  "true" => Tokens.TRUE(!pos,!pos)
				| "false" => Tokens.FALSE(!pos,!pos)
				| "let" => Tokens.LET(!pos,!pos)
				| "letfun" => Tokens.LETF(!pos,!pos)
				| "letrec" => Tokens.LETR(!pos,!pos)
				| "lettype" => Tokens.LETTY(!pos,!pos)
				| "datatype" => Tokens.LETDT(!pos,!pos)
				| "in" => Tokens.IN(!pos,!pos)
				| "fix" => Tokens.FIX(!pos,!pos)
				| "case" => Tokens.CASE(!pos,!pos)
				| "of" => Tokens.OF(!pos,!pos)
				| "if" => Tokens.IF(!pos,!pos)
				| "then" => Tokens.THEN(!pos,!pos)
				| "else" => Tokens.ELSE(!pos,!pos)
				| "fn" => Tokens.FN(!pos,!pos)
				| "next" => Tokens.NEXT(!pos,!pos)
				| "prev" => Tokens.PREV(!pos,!pos)
				| "mono" => Tokens.MONO(!pos,!pos)
				| "inl" => Tokens.INL(!pos,!pos)
				| "inr" => Tokens.INR(!pos,!pos)
				| "roll" => Tokens.ROLL(!pos,!pos)
				| "unroll" => Tokens.UNROLL(!pos,!pos)
				| "hold" => Tokens.HOLD(!pos,!pos)
				| "int" => Tokens.INT(!pos,!pos)
				| "bool" => Tokens.BOOL(!pos,!pos)
				| "str" => Tokens.STR(!pos,!pos)
				| "unit" => Tokens.UNIT(!pos,!pos)
				| "mu" => Tokens.MU(!pos,!pos)
				| "mod" => Tokens.MOD(!pos,!pos)
				| "push" => Tokens.PUSH(!pos,!pos)
				| "pushA" => Tokens.PUSHA(!pos,!pos)
				| "pushP" => Tokens.PUSHP(!pos,!pos)
				| "pushS" => Tokens.PUSHS(!pos,!pos)
				| s => Tokens.ID(s,!pos,!pos)
            );
<L12>"-"      => (Tokens.SUB(!pos,!pos));
<L12>"^"      => (Tokens.CARAT(!pos,!pos));
<L12>"/"      => (Tokens.DIV(!pos,!pos));
<L12>"{"      => (Tokens.LBRACE(!pos,!pos));
<L12>"}"      => (Tokens.RBRACE(!pos,!pos));
<L12>"("      => (Tokens.LPAR(!pos,!pos));
<L12>")"      => (Tokens.RPAR(!pos,!pos));
<L12>"|"      => (Tokens.BAR(!pos,!pos));
<L12>"."      => (error ("ignoring bad character "^yytext,!pos,!pos); lex());
<L12>\"(\\.|[^\\"])*\" => (Tokens.STRLIT (yytext,!pos,!pos));
<CMT>{eol}    => (pos:=(!pos)+1; YYBEGIN L12; continue ());
<CMT>.        => (continue ());