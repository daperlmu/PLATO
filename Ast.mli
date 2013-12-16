type operator = 
	| Not
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
	| BooleanType
	| NumberType of string

type platoFunctionType =
	| VoidType
	| BooleanType
	| NumberType of string

type expression = 
	| Boolean of bool
	| Number of int
	| Identifier of string
	| Unop of operator * expression
	| Binop of operator * expression * expression

type  statement =
	| Print of expression
  | Assignment of string * expression
	| Declaration of platoType * string * expression	
				
type statementBlock = 
	  StatementBlock of statement list

type parameter = Parameter of platoType * string

type functionHeader = {
    returnType : platoFunctionType;
    functionName : string;
    parameters : parameter list;
  }

type functionBlock = 
	  FunctionDeclaration of functionHeader * statementBlock

type mainBlock = 
	  MainBlock of statementBlock
		
type program  =
	  Program of mainBlock * functionBlock list