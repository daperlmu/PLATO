open Ast;;

type  variableDeclaration = 
	  string * Ast.platoType
type functionDeclaration = 
		string * Ast.platoType * Ast.parameter list

type typedExpression = 
	| TypedBoolean of bool * Ast.platoFunctionType
	| TypedNumber of int * Ast.platoFunctionType
  | TypedIdentifier of string  * Ast.platoFunctionType
	| TypedUnop of operator * Ast.platoFunctionType * typedExpression
	| TypedBinop of operator * Ast.platoFunctionType * typedExpression * typedExpression
	| TypedSet of Ast.platoFunctionType * typedExpression list
	| TypedFunctionCall of Ast.platoFunctionType * string * typedExpression list

type typedStatement = 
	| TypedPrint of typedExpression
	| TypedReturn of Ast.platoFunctionType * typedExpression
	| TypedIf of Ast.platoFunctionType * typedExpression * typedStatementBlock * typedElseIfBlock list * typedElseBlock
	| TypedIfNoElse of typedExpression * typedStatementBlock * typedElseIfBlock list
	| TypedAssignment of variableDeclaration * typedExpression      
	| TypedDeclaration of variableDeclaration * typedExpression
and typedElseIfBlock = 
	TypedElseIfBlock of typedExpression * typedStatementBlock
and typedElseBlock = 
	TypedElseBlock of typedStatementBlock
and typedStatementBlock = 
	  TypedStatementBlock of typedStatement list

type typedParameter = 
	  TypedParameter of variableDeclaration
	  
type typedFunctionBlock =
	  TypedFunctionDeclaration of Ast.functionHeader * typedStatementBlock

type typedMainBlock = 
	  TypedMainBlock of typedStatementBlock	
																		
type typedExtendedGroupBlock =
	| TypedGroupDeclaration of string * int list * int list list * int list
	| TypedRingDeclaration of string * int list * int list list * int list	* int list list	
  | TypedFieldDeclaration of string * int list * int list list * int list	* int list list * int list * int	

type typedProgram = 
	  TypedProgram of typedMainBlock * typedFunctionBlock list * typedExtendedGroupBlock list
