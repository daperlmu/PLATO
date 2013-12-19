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
	| SetDifference

type platoType =
	| BooleanType
	| NumberType of string
	| SetLiteralType of platoType
	| NeutralType

type platoQuantifier =
	| WhichQuantifier
	| SomeQuantifier
	| AllQuantifier

type platoFunctionType =
	| VoidType
	| OtherType of platoType

type expression = 
	| Boolean of bool
	| Number of int
	| Identifier of string
	| Unop of operator * expression
	| Binop of operator * expression * expression
	| SetLiteral of expression list
	| FunctionCall of string * expression list

type statement =
	| Print of expression
	| Return of expression
	| If of expression * statementBlock * elseIfBlock list * elseBlock
	| IfNoElse of expression * statementBlock * elseIfBlock list
  | Assignment of string * expression
	| Declaration of platoType * string * expression	
and statementBlock = 
	  StatementBlock of statement list
and elseBlock = 
	ElseBlock of statementBlock
and elseIfBlock = 
	ElseIfBlock of expression * statementBlock

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
		
type groupHeader = 
	  GroupHeader of string			

type groupBody =
	  GroupBody of expression * functionBlock			
		
type extendedGroupHeader = 
	| RingHeader of string
	| FieldHeader of string			
		
(* TODO should be int set not int list *)	
type extendedGroupBody =
	  ExtendedGroupBody of groupBody * functionBlock			
																		
type extendedGroupBlock =
	| GroupDeclaration of groupHeader * groupBody
  | ExtendedGroupDeclaration of extendedGroupHeader * extendedGroupBody		

type program  =
	  Program of mainBlock * functionBlock list * extendedGroupBlock list
