open Printf;;
open Ast;;
open Sast;;
open JavaAst;;

let operatorToString = function
	  Not ->  "not"
	| And -> "and"
	| Or -> "or"
	| Negation -> "negation"
	| Plus -> "plus"
	| Minus -> "minus"
	| Times -> "times"
	| Divide -> "divide"
	| Mod -> "mod"
	| Raise -> "raise"
	| LessThan -> "lessThan"
	| LessThanOrEqual -> "lessThanOrEqual"
	| GreaterThan -> "greaterThan"
	| GreaterThanOrEqual -> "greaterThanOrEqual"
	| Equal ->  "equal"

let logToFile mode permissions newline fileName logString =
	let fileHandle = open_out_gen mode permissions fileName
	in (if newline
		 then fprintf fileHandle "%s\n"  logString
		 else fprintf fileHandle "%s" logString);
		 close_out fileHandle
	
let logToFileAppend = logToFile [Open_creat; Open_append] 0o777

let logToFileOverwrite = logToFile [Open_creat; Open_wronly] 0o777

(* Logging for PLATO AST *)
let logListToAst logStringList = 
	(logToFileAppend true) "Ast.log" (String.concat " " logStringList)
	
let logStringToAst logString = 
	logListToAst [logString]

let logOperatorAst operator = logStringToAst (operatorToString operator)

let rec logPlatoTypeAst = function
	| BooleanType -> logStringToAst "BooleanType"
	| NumberType(gropuName) -> logListToAst ["Number Type over group "; gropuName]

let rec logExpressionAst = function
	| Boolean(booleanValue) -> logListToAst ["Boolean"; string_of_bool booleanValue]
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

let typeToString = function
	| BooleanType -> "Booleans"
	| NumberType(groupName) -> groupName

let logListToSast logStringList = 
	(logToFileAppend true) "Sast.log" (String.concat " " logStringList)
	
let logStringToSast logString = 
	logListToSast [logString]
	
let logOperatorSast operator = logStringToSast (operatorToString operator)	

let logPlatoTypeSast = function
	| BooleanType -> logStringToSast "Boolean Type"
	| NumberType(groupName) -> logListToSast ["Number Type over group"; groupName]

let rec logExpressionSast = function
	| TypedBoolean(booleanValue, _) -> logListToSast ["Boolean"; string_of_bool booleanValue]
	| TypedNumber(numberValue, numberType) -> logListToSast ["Number"; string_of_int numberValue; " over "; typeToString numberType]
  | TypedIdentifier(variableName, variableType) -> logListToSast ["Variable";  variableName; "of type"; typeToString variableType]
	| TypedUnop(unaryOperator, operatorType, operatorExpression) -> 
		logStringToSast "unary operator"; 
		logOperatorSast unaryOperator;
		logListToSast ["of type";  typeToString operatorType];
		logStringToSast "acting on"; 
		logExpressionSast operatorExpression;
	| TypedBinop(binaryOperator, operatorType, operatorExpression1, operatorExpression2) -> 
		logStringToSast "binary operator"; 
		logListToSast ["of type";  typeToString operatorType];
		logOperatorSast binaryOperator;
		logStringToSast "acting on";
		logExpressionSast operatorExpression1;
		logStringToSast "and acting on";
		logExpressionSast operatorExpression2

let logStatementSast = function
	  TypedPrint(printExpression) -> 
			logStringToSast "Print"; 
			logExpressionSast(printExpression)
	| TypedAssignment((variableName, variableType), newValue) -> 
		logListToSast ["Assign"; variableName; "of type"]; 
		logPlatoTypeAst variableType;
		logStringToSast " to value "; 
		logExpressionSast(newValue)
	| TypedDeclaration((variableName, variableType), newValue) ->
	  logListToSast ["Declare"; variableName;  "as type"]; 
		logPlatoTypeSast variableType;
		logStringToSast " and assign to value "; 
		logExpressionSast(newValue)

let logStatementBlockSast = function
	  TypedStatementBlock(statementList) -> logListToSast ["StatementBlock of size"; string_of_int (List.length statementList)]; ignore (List.map logStatementSast statementList)

let logMainBlockSast = function
	  TypedMainBlock(statementBlock) -> logStringToSast "MainBlock"; logStatementBlockSast statementBlock
	
let logProgramSast = function
    TypedProgram(mainBlock) -> logListToSast ["Program of size"; "1"]; logMainBlockSast mainBlock
		
(* Logging for Java AST *)
let logListToJavaAst logStringList = 
	(logToFileAppend true) "JavaAst.log" (String.concat " " logStringList)
	
let logStringToJavaAst logString = 
	logListToJavaAst [logString]

let rec logJavaExpressionAst = function
	| JavaBoolean(booleanValue) -> logListToJavaAst ["Java boolean"; string_of_bool booleanValue]
  | JavaInt(intValue) -> logListToJavaAst ["Java int"; string_of_int intValue]
	| JavaVariable(stringValue) -> logListToJavaAst ["Java variable"; stringValue]
	| JavaAssignment(variableName, variableValue) -> 
		logListToJavaAst ["Java assignment of variable"; variableName; "to"];
		logJavaExpressionAst variableValue
	| JavaDeclaration(variableType, variableName, variableValue) ->
	  (logListToJavaAst ["Java assignment of variable"; variableName; "assigned to"];
		match variableValue with 
		    Some(javaExpressionValue) -> logJavaExpressionAst javaExpressionValue
	    | None -> () (* do nothing *))
	| JavaCall(className, methodName, methodParameters) -> logListToJavaAst ["Java call to"; className; "."; methodName; "with"; string_of_int (List.length methodParameters); "parameters"]; ignore (List.map logJavaExpressionAst methodParameters)

let logJavaStatementAst = function
    JavaStatement(javaExpression) -> logStringToJavaAst "Java bare statement"; logJavaExpressionAst javaExpression

let logJavaBlockAst = function
	  JavaBlock(statementList) -> logListToJavaAst ["Java block with"; string_of_int (List.length statementList); "statements"]; ignore (List.map logJavaStatementAst statementList)

let logJavaMethodAst = function
	  JavaMain(methodBlock) -> logStringToJavaAst "Java main"; logJavaBlockAst methodBlock

let logJavaClassAst = function
	  JavaClass(className, javaMethodList) -> logListToJavaAst ["Java class"; className; "with "; string_of_int (List.length javaMethodList); "methods"]; 
		ignore (List.map logJavaMethodAst javaMethodList)
	
let logJavaClassListAst = function
    JavaClassList(javaClassList) -> logListToJavaAst ["Java class list of size"; string_of_int (List.length javaClassList)]; ignore (List.map logJavaClassAst javaClassList)