open Sast;;

type javaType =
	| JavaBooleanType
	| JavaIntType
	| JavaSetLiteralType
	| JavaVectorLiteralType
	| JavaCasesLiteralType
	| JavaNeutralType

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
	| JavaIfNoElse of javaExpression * javaBlock * javaElseIf list
	| JavaAssignment of string * javaExpression
	| JavaVectorAssignment of string * javaExpression * javaExpression
	| JavaDeclaration of javaType * string * javaExpression option
	| JavaCall of string * string * javaExpression list
	| JavaTernaryChain of (javaExpression * javaExpression) list * javaExpression
and javaElseIf = 
	JavaElseIf of javaExpression * javaBlock
and javaElse = 
	JavaElse of javaBlock
and javaStatement =
	  JavaStatement of javaExpression
and javaBlock =
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
