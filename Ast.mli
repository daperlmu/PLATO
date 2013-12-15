type operator = 
	  Not
	| And
	| Or
	| Negation
  | Plus 
	| Minus
	| Times
	| Divide
	| Mod
	| Raise
	| LessThan
	| LessThanOrEqual
	| GreaterThan
	| GreaterThanOrEqual
	| Equal

type platoType =
	  BooleanType
  | IntegerType
	| NumberType of string

type expression = 
	  Boolean of bool
	| Number of int
	| Identifier of string
	| Unop of operator * expression
	| Binop of operator * expression * expression

type  statement =
	  Print of expression
  | Assignment of string * expression
	| Declaration of platoType * string * expression	
				
type statementBlock = 
	  StatementBlock of statement list
		
type mainBlock = 
	  MainBlock of statementBlock
		
type program  =
	  Program of mainBlock