open Printf;;
open Ast;;
open Sast;;
open JavaAst;;

let logToFileNoNewline fileName logString = 
	let fileHandle = open_out_gen [Open_creat; Open_append] 0o777 fileName
	in fprintf fileHandle "%s" logString; close_out fileHandle

let logToFile fileName logString = 
	let fileHandle = open_out_gen [Open_creat; Open_append] 0o777 fileName
	in fprintf fileHandle "%s\n" logString; close_out fileHandle

(* Logging for PLATO AST *)
let logListToAst logStringList = 
	logToFile "Ast.log" (String.concat " " logStringList)
	
let logStringToAst logString = 
	logListToAst [logString]

let logOperatorAst = function
	  Not ->  logStringToAst "Not"
	| And -> logStringToAst "And"
	| Or -> logStringToAst "Or"
	| Plus -> logStringToAst "Plus"
	| Minus -> logStringToAst "Minus"
	| Times -> logStringToAst "Times"
	| Divide -> logStringToAst "Divide"
	| Mod -> logStringToAst "Mod"
	| Raise -> logStringToAst "Raise"
	| LessThan -> logStringToAst "LessThan"
	| LessThanOrEqual -> logStringToAst "LessThanOrEqual"
	| GreaterThan -> logStringToAst "GreaterThan"
	| GreaterThanOrEqual -> logStringToAst "GreaterThanOrEqual"
	| Equal ->  logStringToAst "Equal"

let rec logPlatoTypeAst = function
	  BooleanType -> logStringToAst "BooleanType"
	| IntegerType -> logStringToAst "IntegerType"
	| NumberType(identifier) -> logListToAst ["NumberType"; "Over"; identifier]

let rec logExpressionAst = function
	  Boolean(booleanValue) -> logListToAst ["Boolean"; string_of_bool booleanValue]
	| Number(integerValue) -> logListToAst ["Number"; string_of_int integerValue]
	| Identifier(identifierName) -> logListToAst ["Identifier"; identifierName]
	| Unop(operator, expression) -> logOperatorAst operator; logExpressionAst expression
	| Binop(operator, expression1, expression2) -> logOperatorAst operator; logExpressionAst expression1; logExpressionAst expression2

let logStatementAst = function
	  Print(printValue) -> logStringToAst "Print"; logExpressionAst(printValue)
	| Assignment(identifier, rhs) -> logStringToAst "Assignment"; logListToAst ["Identifier"; identifier]; logExpressionAst(rhs)
	| Declaration(platoType, identifier, rhs) -> logStringToAst "Declaration"; logPlatoTypeAst platoType; logListToAst ["Identifier"; identifier]; logExpressionAst(rhs)
		
let logStatementBlockAst = function
	  StatementBlock(statementList) -> logListToAst ["StatementBlock of size"; string_of_int (List.length statementList)]; ignore (List.map logStatementAst statementList)
		
let logMainBlockAst = function
	  MainBlock(statementBlock) -> logStringToAst "MainBlock"; logStatementBlockAst statementBlock

let logProgramAst = function
	  Program(mainBlock) -> logListToAst ["Program of size"; "1"]; logMainBlockAst mainBlock
		
(* Logging for PLATO SAST *)
let logListToSast logStringList = 
	logToFile "Sast.log" (String.concat " " logStringList)
	
let logStringToSast logString = 
	logListToSast [logString]
		
let rec logExpressionSast = function
	  TypedBoolean(booleanValue) -> logListToAst ["Boolean"; string_of_bool booleanValue]
	| TypedNumber(integerValue) -> logListToAst ["Number"; string_of_int integerValue]
  | TypedIdentifier(variableName) -> logListToAst ["Variable";  variableName]

let logStatementSast = function
	  TypedPrint(printValue, printType) -> 
			logStringToSast "Print"; 
			logExpressionSast(printValue); 
			logPlatoTypeAst printType
	| TypedAssignment((variableName, variableType), (newValue, newValueType)) -> 
		logListToSast ["Assign"; variableName; "of type"]; 
		logPlatoTypeAst variableType;
		logStringToSast " to value "; 
		logExpressionSast(newValue);
	  logStringToSast " of type ";  
		logPlatoTypeAst newValueType
	| TypedDeclaration((variableName, variableType), (newValue, newValueType)) ->
	  logListToSast ["Declare"; variableName;  "as type"]; 
		logPlatoTypeAst variableType;
		logStringToSast " and assign to value "; 
		logExpressionSast(newValue);
	  logStringToSast " of type ";  
		logPlatoTypeAst newValueType

let logStatementBlockSast = function
	  TypedStatementBlock(statementList) -> logListToSast ["StatementBlock of size"; string_of_int (List.length statementList)]; ignore (List.map logStatementSast statementList)

let logMainBlockSast = function
	  TypedMainBlock(statementBlock) -> logStringToSast "MainBlock"; logStatementBlockSast statementBlock
	
let logProgramSast = function
    TypedProgram(mainBlock) -> logListToSast ["Program of size"; "1"]; logMainBlockSast mainBlock
		
(* Logging for Java AST *)
let logListToJavaAst logStringList = 
	logToFile "JavaAst.log" (String.concat " " logStringList)
	
let logStringToJavaAst logString = 
	logListToJavaAst [logString]

let rec logJavaCallAst = function
    JavaCall(methodName, methodParameters) -> logListToJavaAst ["Java call to"; methodName; "with"; string_of_int (List.length methodParameters); "parameters"]; ignore (List.map logJavaExpressionAst methodParameters)
and logJavaExpressionAst = function
	  JavaBoolean(booleanValue) -> logListToJavaAst ["Java int"; string_of_bool booleanValue]
  | JavaInt(intValue) -> logListToJavaAst ["Java int"; string_of_int intValue]
	| JavaVariable(stringValue) -> logListToJavaAst ["Java variable"; stringValue]
  | JavaExpression(javaCall) -> logJavaCallAst javaCall
	| JavaAssignment(variableName, variableValue) -> 
		logListToJavaAst ["Java assignment of variable"; variableName; "to"];
		logJavaExpressionAst variableValue
	| JavaDeclaration(variableType, variableName, variableValue) ->
	  logListToJavaAst ["Java assignment of variable"; variableName; "assigned to"];
		match variableValue with 
		    Some(javaExpressionValue) -> logJavaExpressionAst javaExpressionValue
	    | None -> () (* do nothing *)

let logJavaStatementAst = function
    JavaStatement(javaExpression) -> logStringToJavaAst "Java bare statement"; logJavaExpressionAst javaExpression

let logJavaBlockAst = function
	  JavaBlock(statementList) -> logListToJavaAst ["Java block with"; string_of_int (List.length statementList); "statements"]; ignore (List.map logJavaStatementAst statementList)

let logJavaMethodAst = function
	  JavaMain(methodBlock) -> logStringToJavaAst "Java main"; logJavaBlockAst methodBlock

let logJavaClassAst = function
	  JavaClass(className, javaMethodList) -> logListToJavaAst ["Java class"; className; "with"; string_of_int (List.length javaMethodList); "methods"]; ignore (List.map logJavaMethodAst javaMethodList)
	
let logJavaClassListAst = function
    JavaClassList(javaClassList) -> logListToJavaAst ["Java class list of size"; string_of_int (List.length javaClassList)]; ignore (List.map logJavaClassAst javaClassList)