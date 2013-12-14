open Ast;;

type  variableDeclaration = 
	  string * Ast.platoType

type expressionDetail = 
	  TypedBoolean of bool
	| TypedNumber of int
  | TypedIdentifier of string
		
type typedExpression =
	  expressionDetail * Ast.platoType

type typedStatement = 
	  TypedPrint of typedExpression
	| TypedAssignment of variableDeclaration * typedExpression      
	| TypedDeclaration of variableDeclaration * typedExpression
	
type typedStatementBlock = 
	  TypedStatementBlock of typedStatement list
				
type typedMainBlock = 
	  TypedMainBlock of typedStatementBlock
		
type typedProgram = 
	  TypedProgram of typedMainBlock
