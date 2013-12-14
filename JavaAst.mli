open Sast;;

type javaType =
	  Bool
	| Int

type javaCall = 
	  JavaCall of string * javaExpression list
and 
javaExpression = 
	  JavaBoolean of bool
	| JavaInt of int
  | JavaVariable of string
	| JavaUnop of javaOperator * javaExpression
	| JavaBinop of javaOperator * javaExpression * javaExpression
	| JavaExpression of javaCall
	| JavaAssignment of string * javaExpression
	| JavaDeclaration of javaType * string * javaExpression option
and 
javaOperator = 
	  JavaNot
	| JavaAnd
	| JavaOr
  | JavaPlus 
	| JavaMinus
	| JavaTimes
	| JavaDivide
	| JavaMod
	| JavaOperator of javaCall
	| JavaLessThan
	| JavaLessThanOrEqual
	| JavaGreaterThan
	| JavaGreaterThanOrEqual
	| JavaEqual

type javaStatement =
	  JavaStatement of javaExpression

type javaBlock =
	  JavaBlock of javaStatement list

type javaMethod	=
	  JavaMain of javaBlock
				
type javaClass = 
	  JavaClass of string * javaMethod list
		
type javaClassList = 
	  JavaClassList of javaClass list
