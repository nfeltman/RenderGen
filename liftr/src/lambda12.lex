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
digit=[0-9];
idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.NUM
                (List.foldl (fn (a,r) => ord(a)-ord(#"0")+10*r)
                         0 (explode yytext),
                  !pos,!pos));
"+"      => (Tokens.PLUS(!pos,!pos));
"*"      => (Tokens.TIMES(!pos,!pos));
":"      => (Tokens.COLON(!pos,!pos));
";"      => (Tokens.SEMI(!pos,!pos));
","      => (Tokens.COMMA(!pos,!pos));
"$"      => (Tokens.DOLLAR(!pos,!pos));
"="      => (Tokens.EQ(!pos,!pos));
"->"     => (Tokens.ARROW(!pos,!pos));
"=>"     => (Tokens.DARROW(!pos,!pos));
">"      => (Tokens.GT(!pos,!pos));
"#1"     => (Tokens.PROJL(!pos,!pos));
"#2"     => (Tokens.PROJR(!pos,!pos));
{id}     => (case yytext of
				  "let" => Tokens.LET(!pos,!pos)
				| "in" => Tokens.IN(!pos,!pos)
				| "case" => Tokens.CASE(!pos,!pos)
				| "of" => Tokens.OF(!pos,!pos)
				| "if" => Tokens.IF(!pos,!pos)
				| "then" => Tokens.THEN(!pos,!pos)
				| "else" => Tokens.ELSE(!pos,!pos)
				| "fn" => Tokens.FN(!pos,!pos)
				| "next" => Tokens.NEXT(!pos,!pos)
				| "prev" => Tokens.PREV(!pos,!pos)
				| "inl" => Tokens.INL(!pos,!pos)
				| "inr" => Tokens.INR(!pos,!pos)
				| "hold" => Tokens.HOLD(!pos,!pos)
				| "int" => Tokens.INT(!pos,!pos)
				| "bool" => Tokens.BOOL(!pos,!pos)
				| "unit" => Tokens.UNIT(!pos,!pos)
				| s => Tokens.ID(s,!pos,!pos)
            );
"-"      => (Tokens.SUB(!pos,!pos));
"^"      => (Tokens.CARAT(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
"{"      => (Tokens.LBRACE(!pos,!pos));
"}"      => (Tokens.RBRACE(!pos,!pos));
"("      => (Tokens.LPAR(!pos,!pos));
")"      => (Tokens.RPAR(!pos,!pos));
"|"      => (Tokens.BAR(!pos,!pos));
"."      => (error ("ignoring bad character "^yytext,!pos,!pos);
             lex());