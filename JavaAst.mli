open Sast;;

type javaType =
	| JavaBooleanType
	| JavaIntType

type javaExpression = 
	| JavaBoolean of bool
	| JavaInt of int
  | JavaVariable of string
	| JavaAssignment of string * javaExpression
	| JavaDeclaration of javaType * string * javaExpression option
	| JavaCall of string * string * javaExpression list

type javaStatement =
	  JavaStatement of javaExpression

type javaBlock =
	  JavaBlock of javaStatement list

(* type javaFunctionHeader = 
	  JavaFunctionHeader of javaFunctionType * string*)

type javaMethod	=
	  JavaMain of javaBlock
	  | JavaFunction of Ast.functionHeader * javaBlock
				
type javaClass = 
	  JavaClass of string * javaMethod list
		
type javaClassList = 
	  JavaClassList of javaClass list
