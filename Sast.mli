open Ast;;

type  variableDeclaration = 
	  string * Ast.platoType

type typedExpression = 
	| TypedBoolean of bool * Ast.platoType
	| TypedNumber of int * Ast.platoType
  | TypedIdentifier of string  * Ast.platoType
	| TypedUnop of operator * Ast.platoType * typedExpression
	| TypedBinop of operator * Ast.platoType * typedExpression * typedExpression
	| TypedSet of Ast.platoType * typedExpression list

type typedStatement = 
	| TypedPrint of typedExpression
	(*| TypedReturn of typedExpression*)
	| TypedAssignment of variableDeclaration * typedExpression      
	| TypedDeclaration of variableDeclaration * typedExpression

type typedStatementBlock = 
	  TypedStatementBlock of typedStatement list

type typedParameter = 
	  TypedParameter of variableDeclaration
	  
type typedFunctionBlock =
	  TypedFunctionDeclaration of Ast.functionHeader * typedStatementBlock

type typedMainBlock = 
	  TypedMainBlock of typedStatementBlock	
																		
type typedGroupBlock =
    TypedGroupDeclaration of string * int list list * int list

type typedProgram = 
	  TypedProgram of typedMainBlock * typedFunctionBlock list * typedGroupBlock list
