open Sast;;

type javaType =
	| JavaBooleanType
	| JavaIntType

type javaPrimitive = 
	| JavaBoolean of bool
	| JavaInt of int

type javaValue =
	| JavaValue of javaPrimitive
	| JavaMap of string list * string list

type javaExpression = 
	| JavaConstant of javaValue
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

type javaInstanceVariable =
	  JavaInstanceVariable of string * string * javaValue

type javaMethod	=
	  | JavaMain of javaBlock
	  | JavaFunction of Ast.functionHeader * javaBlock
						
type javaClass = 
	  JavaClass of string * string * javaInstanceVariable list * javaMethod list
		
type javaClassList = 
	  JavaClassList of javaClass list
