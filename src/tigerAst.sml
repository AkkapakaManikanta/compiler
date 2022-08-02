structure TigerAst = struct

	datatype Inst = Assign of string * Expr
		      | Print of Expr
		      | For of string * int * int * Inst list
		      
		and Expr = Const of int
			 | Var of string
			 | Op of Expr * BinOp * Expr
			 
		and BinOp = Plus
			  | Minus
			  | Mul
			  | Div
			  
fun plus   a b = Op (a, Plus , b)
fun minus  a b = Op (a, Minus, b)
fun mul    a b = Op (a, Mul  , b)
fun divi   a b = Op (a, Div  , b)

end
