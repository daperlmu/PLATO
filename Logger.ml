open Printf;;
open Ast;;
open Sast;;
open JavaAst;;

let operatorToString = function
	  Not ->  "Not"
	| And -> "And"
	| Or -> "Or"
	| Negation -> "Negation"
	| Plus -> "Plus"
	| Minus -> "Minus"
	| Times -> "Times"
	| Divide -> "Divide"
	| Mod -> "Mod"
	| Raise -> "Raise"
	| LessThan -> "LessThan"
	| LessThanOrEqual -> "LessThanOrEqual"
	| GreaterThan -> "GreaterThan"
	| GreaterThanOrEqual -> "GreaterThanOrEqual"
	| Equal ->  "Equal"

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

let typeToString = function
	  BooleanType -> "boolean"
  | IntegerType -> "integer"
	| NumberType(groupName) -> "number"

let logListToSast logStringList = 
	(logToFileAppend true) "Sast.log" (String.concat " " logStringList)
	
let logStringToSast logString = 
	logListToSast [logString]
	
let logOperatorSast operator = logStringToSast (operatorToString operator)	

let logPlatoTypeSast = function
		BooleanType -> logStringToSast "Boolean Type"
  | IntegerType -> logStringToSast "Integer Type"
	| NumberType(groupName) -> logListToSast ["Number Type over group"; groupName]

let rec logExpressionSast = function
	  TypedBoolean(booleanValue, _) -> logListToSast ["Boolean"; string_of_bool booleanValue]
	| TypedNumber(integerValue, _) -> logListToSast ["Number"; string_of_int integerValue]
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

let rec logJavaCallAst = function
    JavaCall(methodName, methodParameters) -> logListToJavaAst ["Java call to"; methodName; "with"; string_of_int (List.length methodParameters); "parameters"]; ignore (List.map logJavaExpressionAst methodParameters)
and logJavaExpressionAst = function
	  JavaBoolean(booleanValue) -> logListToJavaAst ["Java boolean"; string_of_bool booleanValue]
  | JavaInt(intValue) -> logListToJavaAst ["Java int"; string_of_int intValue]
	| JavaVariable(stringValue) -> logListToJavaAst ["Java variable"; stringValue]
	| JavaUnop(unaryOperator, unopExpression) -> 
		logStringToJavaAst "Unary operator"; 
		logOperatorJavaAst unaryOperator; 
		logJavaExpressionAst unopExpression
	| JavaBinop(binaryOperator, binopExpression1, binopExpression2) -> 
		logStringToJavaAst "Binary operator"; 
		logOperatorJavaAst binaryOperator; 
		logJavaExpressionAst binopExpression1;
	  logJavaExpressionAst binopExpression2
  | JavaExpression(javaCall) -> logJavaCallAst javaCall
	| JavaAssignment(variableName, variableValue) -> 
		logListToJavaAst ["Java assignment of variable"; variableName; "to"];
		logJavaExpressionAst variableValue
	| JavaDeclaration(variableType, variableName, variableValue) ->
	  logListToJavaAst ["Java assignment of variable"; variableName; "assigned to"];
		match variableValue with 
		    Some(javaExpressionValue) -> logJavaExpressionAst javaExpressionValue
	    | None -> () (* do nothing *)
and logOperatorJavaAst = function
	  JavaNot -> logStringToJavaAst "java not"
	| JavaAnd -> logStringToJavaAst "java and"
	| JavaOr -> logStringToJavaAst "java or"
	| JavaNegation -> logStringToJavaAst "java negation"
  | JavaPlus -> logStringToJavaAst "java plus"
	| JavaMinus -> logStringToJavaAst "java minus"
	| JavaTimes -> logStringToJavaAst "java times"
	| JavaDivide -> logStringToJavaAst "java divide"
	| JavaMod -> logStringToJavaAst "java mod"
	| JavaOperator(javaCall) -> logJavaCallAst javaCall
	| JavaLessThan -> logStringToJavaAst "java less than"
	| JavaLessThanOrEqual -> logStringToJavaAst "java less than or equal"
	| JavaGreaterThan -> logStringToJavaAst "java greater than"
	| JavaGreaterThanOrEqual -> logStringToJavaAst "java greater than or equal"
	| JavaEqual -> logStringToJavaAst "equal"


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