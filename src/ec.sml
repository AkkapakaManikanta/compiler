
structure EC =
struct

structure TigerAstLrVals = TigerAstLrValsFun(structure Token = LrParser.Token)
structure TigerAstLex    = TigerAstLexFun(structure Tokens = TigerAstLrVals.Tokens)
structure TigerAstParser = Join( structure ParserData = TigerAstLrVals.ParserData
			     structure Lex        = TigerAstLex
			     structure LrParser   = LrParser
			   )

fun makeTigerAstLexer strm = TigerAstParser.makeLexer (fn n => TextIO.inputN(strm,n))
val makeFileLexer      = makeTigerAstLexer o TextIO.openIn



val thisLexer = case CommandLine.arguments() of
		    []  => makeTigerAstLexer TextIO.stdIn
		 |  [x] => makeFileLexer x
		 |  _   => (TextIO.output(TextIO.stdErr, "usage: ec file"); OS.Process.exit OS.Process.failure)



fun print_error (s,i:int,_) = TextIO.output(TextIO.stdErr, "Error, line " ^ (Int.toString i) ^ ", " ^ s ^ "\n")

val (program,_) = TigerAstParser.parse (0,thisLexer,print_error,()) 
val executable  = IR.translatetomips program                     
val _           = TextIO.output(TextIO.stdOut, MIPS.prProg executable)
			      

end
