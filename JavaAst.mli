open Sast;;

type javaType =
	| JavaBooleanType
	| JavaIntType
	| JavaSetLiteralType

type javaPrimitive = 
	| JavaBoolean of bool
	| JavaInt of int

type javaValue =
	| JavaValue of javaPrimitive
	| JavaMap of string * string list * string list

type javaExpression = 
	| JavaConstant of javaValue
  | JavaVariable of string
  | JavaReturn of javaExpression
  | JavaIf of javaExpression * javaBlock * javaElseIf list * javaElse
	| JavaAssignment of string * javaExpression
	| JavaDeclaration of javaType * string * javaExpression option
	| JavaCall of string * string * javaExpression list
and javaElseIf = 
	JavaElseIf of javaExpression * 

type javaStatement =
	  JavaStatement of javaExpression

type javaBlock =
	  JavaBlock of javaStatement list

(* type javaFunctionHeader = 
	  JavaFunctionHeader of javaFunctionType * string*)

type javaMethod	=
	  | JavaMain of javaBlock
		| JavaDefaultConstructor of string * javaBlock
	  | JavaFunction of Ast.functionHeader * javaBlock
						
type javaClass = 
	  JavaClass of string * string * javaMethod list
		
type javaClassList = 
	  JavaClassList of javaClass list
