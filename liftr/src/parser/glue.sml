(* glue.sml *)

structure LrVals =
  L12ParseLrValsFun(structure Token = LrParser.Token)

structure Lexer =
  L12LexFun(structure Tokens = LrVals.Tokens);

structure L12GeneralParser =
  Join(structure LrParser = LrParser
       structure ParserData = LrVals.ParserData
       structure Lex = Lexer)
	   
structure L12Parser = 
struct

open LangCommon

fun parseStream s = 
    let val inStream = s;
        val grab : int -> string = fn 
            n => if TextIO.endOfStream inStream 
                 then ""
                 else TextIO.inputN (inStream,n);

        val printError : string * int * int -> unit = fn 
            (msg,line,col) =>
             print ((*"["^Int.toString line^":"
                                ^Int.toString col^"] "^*)msg^"\n");

        val (tree,rem) = L12GeneralParser.parse 
                     (15,
                     (L12GeneralParser.makeLexer grab),
                     printError,
                     ())
            handle L12GeneralParser.ParseError => raise ParseError;
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;
    in 
		tree
    end
	
fun parseString s = parseStream (TextIO.openString s)
fun parseFile s = parseStream (TextIO.openIn s)


end 