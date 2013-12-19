open Ast;;

type  variableDeclaration = 
	  string * Ast.platoType
type functionDeclaration = 
		string * Ast.platoFunctionType * Ast.parameter list

type typedExpression = 
	| TypedBoolean of bool * Ast.platoType
	| TypedNumber of int * Ast.platoType
  | TypedIdentifier of string  * Ast.platoType
	| TypedUnop of operator * Ast.platoType * typedExpression
	| TypedBinop of operator * Ast.platoType * typedExpression * typedExpression
	| TypedSet of Ast.platoType * typedExpression list
	| TypedVector of Ast.platoType * typedExpression list
	| TypedVectorRange of Ast.platoType * typedExpression * typedExpression * typedExpression
	| TypedFunctionCall of Ast.platoFunctionType * string * typedExpression list

type typedStatement = 
	| TypedPrint of typedExpression
	| TypedReturn of Ast.platoFunctionType * typedExpression
	| TypedIf of Ast.platoFunctionType * typedExpression * typedStatementBlock * typedElseIfBlock list * typedElseBlock
	| TypedIfNoElse of typedExpression * typedStatementBlock * typedElseIfBlock list
	| TypedAssignment of variableDeclaration * typedExpression
	| TypedVectorAssignment of variableDeclaration * typedExpression * typedExpression
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
