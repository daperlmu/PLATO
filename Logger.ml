open Printf;;
open Ast;;
open Sast;;
open JavaAst;;

let operatorToString = function
	| At -> "at"
	| Not ->  "not"
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
	| SetDifference ->  "setDifference"
	| VectorAccess -> "vectorAccess"

let rec typeToString = function
	| BooleanType -> "Booleans"
	| NumberType(extendeGroupType, groupName) -> "Number over " ^ extendeGroupType ^ groupName
	| SetLiteralType(subtype) -> "Set of " ^ (typeToString subtype)
	| VectorLiteralType(subtype) -> "Vector of " ^ (typeToString subtype)
	| NeutralType -> "Neutral Type"

let functionTypeToString = function
	| VoidType -> "void";
	| OtherType(platoType) -> typeToString platoType

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
	| NumberType(extendeGroupType, groupName) -> logListToAst ["Number Type over group "; extendeGroupType; groupName]
	| SetLiteralType(subType) -> ignore (logListToAst ["SetLiteral Type of subtype "]); logPlatoTypeAst subType
	| VectorLiteralType(subType) -> ignore (logListToAst ["VectorLiteral Type of subtype "]); logPlatoTypeAst subType
	| NeutralType -> logStringToAst "NeutralType"

let rec logExpressionAst = function
	| Boolean(booleanValue) -> logListToAst ["Boolean"; string_of_bool booleanValue]
	| Number(integerValue) -> logListToAst ["Number"; string_of_int integerValue]
	| Identifier(identifierName) -> logListToAst ["Identifier"; identifierName]
	| Unop(operator, expression) -> logOperatorAst operator; logExpressionAst expression
	| Binop(operator, expression1, expression2) -> logOperatorAst operator; logExpressionAst expression1; logExpressionAst expression2
	| SetLiteral(expressionList) ->
		logListToAst ["Set of"; string_of_int (List.length expressionList);"elements"];
		ignore (List.map logExpressionAst expressionList)
	| VectorLiteral(expressionList) ->
		logListToAst ["Vector of"; string_of_int (List.length expressionList);"elements"];
		ignore (List.map logExpressionAst expressionList)
	| VectorRange(fromExpression, toExpression, byExpression) ->
		logStringToAst "Vector range from ";
    logExpressionAst fromExpression;
	  logStringToAst " to ";
    logExpressionAst toExpression;
	  logStringToAst " by ";
		logExpressionAst byExpression
	| FunctionCall(functionName, expressionList) -> 
		logListToAst ["Call to"; functionName; "with parameters"];
		ignore (List.map logExpressionAst expressionList)


let rec logStatementAst = function
	| VoidCall(voidFunction) -> 
		logStringToAst "Void call to";
		logExpressionAst voidFunction
	| Print(printValue) -> 
		logStringToAst "Print"; 
		logExpressionAst(printValue)
	| Return(expression) -> 
		logStringToAst "Return"; 
		logExpressionAst(expression)
	| If(predicate, ifBody, elseIfBlocks, elseBlock) ->
	  logListToAst ["If statement with"; string_of_int (List.length elseIfBlocks); "else if blocks and an else block"];
		logExpressionAst predicate;
		logStatementBlockAst ifBody;
		ignore (List.map logElseIfBlockAst elseIfBlocks);
		logElseBlockAst elseBlock
	| IfNoElse(predicate, ifBody, elseIfBlocks) ->
	  logListToAst ["If statement with"; string_of_int (List.length elseIfBlocks); "else if blocks"];
		logExpressionAst predicate;
		logStatementBlockAst ifBody;
		ignore (List.map logElseIfBlockAst elseIfBlocks)
	| Assignment(identifier, rhs) ->  
		logListToAst ["Assignment of identifier"; identifier; "to"]; 
		logExpressionAst rhs
	| VectorAssignment(identifier, indexer, rhs) -> 
		logListToAst ["Vector assignment of identifier"; identifier]; 
		logStringToAst "with indexer";
		logExpressionAst indexer;
		logStringToAst "to";
		logExpressionAst rhs
	| Declaration(platoType, identifier, rhs) -> 
		logStringToAst "Declaration"; logPlatoTypeAst platoType; 
		logListToAst ["Identifier"; identifier]; logExpressionAst(rhs)
and logStatementBlockAst = function
	  StatementBlock(statementList) -> logListToAst ["StatementBlock of size"; string_of_int (List.length statementList)]; ignore (List.map logStatementAst statementList)
and logElseIfBlockAst =  function
	| ElseIfBlock(predicate, elseIfBody) ->
		logExpressionAst predicate;
		logStatementBlockAst elseIfBody;
and logElseBlockAst = function		
	| ElseBlock(elseBody) ->
		logStatementBlockAst elseBody
		
let logMainBlockAst = function
	  MainBlock(statementBlock) -> logStringToAst "MainBlock"; logStatementBlockAst statementBlock

let logParameterAst = function
	| Parameter(parameterType, parameterName) -> logListToAst ["parameter"; parameterName; "of type"; typeToString parameterType]

let logFunctionHeaderAst functionHeader = 
	logListToAst ["Plato function with name"; functionHeader.functionName; "return type"; functionTypeToString functionHeader.returnType];
	ignore (List.map logParameterAst functionHeader.parameters)

let logFunctionBlockAst = function
	| FunctionDeclaration(functionHeader, statementBlock) ->
		logFunctionHeaderAst functionHeader;
		logStatementBlockAst statementBlock

let logGroupHeaderAst = function
	| GroupHeader(groupName) ->	logListToAst ["Group with name "; groupName]

let logExtendedGroupHeaderAst = function
	| RingHeader(groupName) ->	logListToAst ["Ring with name "; groupName]
	| FieldHeader(groupName) ->	logListToAst ["Field with name "; groupName]

let logGroupBodyAst = function
	| GroupBody(elements, addFunctionBlock) -> 
		logStringToAst "Elements ";
		logExpressionAst elements;
		logFunctionBlockAst addFunctionBlock
		
let logExtendedGroupBodyAst = function
	| ExtendedGroupBody(GroupBody(elements, addFunctionBlock), multiplyFunctionBlock) -> 
		logStringToAst "Elements ";
		logExpressionAst elements;
		logFunctionBlockAst addFunctionBlock;
		logFunctionBlockAst multiplyFunctionBlock
		
let logGroupBlockAst = function
	| GroupDeclaration(groupHeader, groupBody) -> 
		logGroupHeaderAst groupHeader;
		logGroupBodyAst groupBody
	| ExtendedGroupDeclaration(extendedGroupHeader, extendedGroupBody) -> 
		logExtendedGroupHeaderAst extendedGroupHeader;
		logExtendedGroupBodyAst extendedGroupBody	

let logProgramAst = function
	  Program(mainBlock, functionBlockList, groupBlockList) -> 
			logListToAst ["Program of size"; "1"]; 
			logMainBlockAst mainBlock;
			ignore (List.map logFunctionBlockAst functionBlockList); 
			ignore (List.map logGroupBlockAst groupBlockList)
		
(* Logging for PLATO SAST *)
let rec typeToString = function
	| BooleanType -> "Booleans"
	| NumberType(_, groupName) -> groupName
	| SetLiteralType(platoType) -> ("Set<" ^ (typeToString platoType) ^ ">")
	| VectorLiteralType(platoType) -> ("Vector<" ^ (typeToString platoType) ^ ">")
	| NeutralType -> "NeutralTypes"

let logListToSast logStringList = 
	(logToFileAppend true) "Sast.log" (String.concat " " logStringList)
	
let logStringToSast logString = 
	logListToSast [logString]
	
let logOperatorSast operator = logStringToSast (operatorToString operator)	

let logPlatoTypeSast = function
	| BooleanType -> logStringToSast "Boolean Type"
	| NumberType(extendedGroupType, groupName) -> logListToSast ["Number Type over group"; extendedGroupType; groupName]
	| SetLiteralType(platoType) -> logStringToSast ("SetLiterals<" ^ (typeToString platoType) ^ ">")
	| VectorLiteralType(platoType) -> logStringToSast ("VectorLiterals<" ^ (typeToString platoType) ^ ">")
	| NeutralType -> logStringToSast "Neutral Type"

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
	| TypedSet(platoType, expressionList) ->
		logStringToSast "set literal";
		logListToSast ["of type";  typeToString platoType];
		logStringToSast "containing the expressions";
		ignore (List.map logExpressionSast expressionList)
	| TypedVector(platoType, expressionList) ->
		logStringToSast "vector literal";
		logListToSast ["of type";  typeToString platoType];
		logStringToSast "containing the expressions";
		ignore (List.map logExpressionSast expressionList)
	| TypedVectorRange(vectorType, fromExpression, toExpression, byExpression) ->
		logStringToSast "vector range ";
		logListToSast ["of type";  typeToString vectorType];
	  logStringToSast " from ";
    logExpressionSast fromExpression;
	  logStringToSast " to ";
    logExpressionSast toExpression;
	  logStringToSast " by ";
		logExpressionSast byExpression
	| TypedFunctionCall(functionType, functionName, typedExpressionList) -> 
		logListToSast ["Call to"; functionName; "of type"; functionTypeToString functionType; "with parameters"];
		ignore (List.map logExpressionSast typedExpressionList)

let rec logStatementSast = function
	| TypedVoidCall(voidFunction) -> 
		logStringToSast "Void call to";
		logExpressionSast voidFunction
	| TypedPrint(printExpression) -> 
			logStringToSast "Print"; 
			logExpressionSast(printExpression)
	| TypedReturn(returnType, returnExpression) ->
			logListToSast ["Return of typ"; functionTypeToString returnType];
			logExpressionSast(returnExpression)
	| TypedIf(returnType, predicate, ifBody, elseIfBlocks, elseBlock) ->
	  logListToSast ["If statement with"; string_of_int (List.length elseIfBlocks); "else if blocks and an else block and return type"; functionTypeToString returnType];
		logExpressionSast predicate;
		logStatementBlockSast ifBody;
		ignore (List.map logElseIfBlockSast elseIfBlocks);
		logElseBlockSast elseBlock
	| TypedIfNoElse(predicate, ifBody, elseIfBlocks) ->
	  logListToSast ["If statement with"; string_of_int (List.length elseIfBlocks); "else if blocks"];
		logExpressionSast predicate;
		logStatementBlockSast ifBody;
		ignore (List.map logElseIfBlockSast elseIfBlocks);
	| TypedAssignment((variableName, variableType), newValue) -> 
		logListToSast ["Assign"; variableName; "of type"]; 
		logPlatoTypeAst variableType;
		logStringToSast " to value "; 
		logExpressionSast(newValue)
	| TypedVectorAssignment((identifier, vectorType), indexer, rhs) -> 
		logListToSast ["Vector assignment of identifier"; identifier; "of type"; typeToString vectorType]; 
		logStringToSast "with indexer";
		logExpressionSast indexer;
		logStringToSast "to";
		logExpressionSast rhs
	| TypedDeclaration((variableName, variableType), newValue) ->
	  logListToSast ["Declare"; variableName;  "as type"]; 
		logPlatoTypeSast variableType;
		logStringToSast "and assign to value "; 
		logExpressionSast(newValue)
and logStatementBlockSast = function
	  TypedStatementBlock(statementList) -> logListToSast ["StatementBlock of size"; string_of_int (List.length statementList)]; ignore (List.map logStatementSast statementList)
and logElseIfBlockSast =  function
	| TypedElseIfBlock(predicate, elseIfBody) ->
		logExpressionSast predicate;
		logStatementBlockSast elseIfBody
and logElseBlockSast = function		
	| TypedElseBlock(elseBody) ->
		logStatementBlockSast elseBody		

let logMainBlockSast = function
	  TypedMainBlock(statementBlock) -> logStringToSast "MainBlock"; logStatementBlockSast statementBlock

let logParameterSast = function
	| Parameter(parameterType, parameterName) -> logListToSast ["parameter"; parameterName; "of type"; typeToString parameterType]

let logFunctionHeaderSast functionHeader = 
	logListToSast ["Plato function with name"; functionHeader.functionName; "return type"; functionTypeToString functionHeader.returnType];
	ignore (List.map logParameterSast functionHeader.parameters)

let logFunctionBlockSast = function
	| TypedFunctionDeclaration(functionHeader, statementBlock) ->
		logFunctionHeaderSast functionHeader;
		logStatementBlockSast statementBlock

let logTableSast tableName table = 
	logStringToSast ("Table " ^ tableName);
	ignore (List.map (fun intList -> logListToSast (List.map string_of_int intList)) table)
		
let logGroupBlockSast = function
	| TypedGroupDeclaration(groupName, groupElements, additionTable, additiveInverseList) ->
		logListToSast ["Group with name "; groupName; "and elements"];
		logListToSast (List.map string_of_int groupElements);
		logTableSast "additionTable" additionTable;
		logListToSast ("additiveInverseList"::(List.map string_of_int additiveInverseList))
	| TypedRingDeclaration(ringName, ringElements, additionTable, additiveInverseList, multiplicationTable) ->
		logListToSast ["Ring with name "; ringName; "and elements"];
		logListToSast (List.map string_of_int ringElements);
		logTableSast "additionTable" additionTable;
		logListToSast ("additiveInverseList"::(List.map string_of_int additiveInverseList));
		logTableSast "multiplicationTable" multiplicationTable;
  | TypedFieldDeclaration(fieldName, fieldElements, additionTable, additiveInverseList, multiplicationTable, multiplicitiveInverseList, additiveIdentity) ->
		logListToSast ["Field with name "; fieldName; "and elements"];
		logListToSast (List.map string_of_int fieldElements);
		logTableSast "additionTable" additionTable;
		logListToSast ("additiveInverseList"::(List.map string_of_int additiveInverseList));
		logTableSast "multiplicationTable" multiplicationTable;
		logListToSast ("multiplicitiveInverseList"::(List.map string_of_int multiplicitiveInverseList));
		logListToSast ["Additive identity"; string_of_int additiveIdentity]
								
let logProgramSast = function
    TypedProgram(mainBlock, typedFunctionBlockList, typedGroupBlockList) -> 
			logListToSast ["Program of size"; "1"]; 
			logMainBlockSast mainBlock;
		  ignore (List.map logFunctionBlockSast typedFunctionBlockList); 
			ignore (List.map logGroupBlockSast typedGroupBlockList)
		
(* Logging for Java AST *)
let logListToJavaAst logStringList = 
	(logToFileAppend true) "JavaAst.log" (String.concat " " logStringList)
	
let logStringToJavaAst logString = 
	logListToJavaAst [logString]

let logjavaPrimitiveAst = function
	| JavaBoolean(booleanValue) -> logListToJavaAst ["Java boolean"; string_of_bool booleanValue]
  | JavaInt(intValue) -> logListToJavaAst ["Java int"; string_of_int intValue]

let rec logJavaValueAst = function
	| JavaValue(javaPrimitive) -> logjavaPrimitiveAst javaPrimitive
	| JavaMap(mapName, keyList, valueList) -> 
		logListToJavaAst ["Java map "; mapName; "with keys"];
		logListToJavaAst keyList;
	  logStringToJavaAst " and values ";
	  logListToJavaAst valueList
		
let rec logJavaExpressionAst = function
	| JavaConstant(javaValue) -> logJavaValueAst javaValue
	| JavaVariable(stringValue) -> logListToJavaAst ["Java variable"; stringValue]
	| JavaReturn(expressionToReturn) ->
		logListToJavaAst ["Return statement"];
		logJavaExpressionAst expressionToReturn
	| JavaAssignment(variableName, variableValue) -> 
		logListToJavaAst ["Java assignment of variable"; variableName; "to"];
		logJavaExpressionAst variableValue
	| JavaDeclaration(variableType, variableName, variableValue) ->
	  (logListToJavaAst ["Java assignment of variable"; variableName; "assigned to"];
		match variableValue with 
		    Some(javaExpressionValue) -> logJavaExpressionAst javaExpressionValue
	    | None -> () (* do nothing *))
	| JavaCall(className, methodName, methodParameters) -> 
		logListToJavaAst ["Java call to"; className; "."; methodName; "with"; string_of_int (List.length methodParameters); "parameters"]; 
		ignore (List.map logJavaExpressionAst methodParameters)
	| _ -> ()

let logJavaStatementAst = function
    JavaStatement(javaExpression) -> logStringToJavaAst "Java bare statement"; logJavaExpressionAst javaExpression

let logJavaBlockAst = function
	  JavaBlock(statementList) -> logListToJavaAst ["Java block with"; string_of_int (List.length statementList); "statements"]; ignore (List.map logJavaStatementAst statementList)

let logJavaFunctionHeaderAst functionHeader =
	logListToAst ["Function with name"; functionHeader.functionName; "return type"; functionTypeToString functionHeader.returnType];
	ignore (List.map logParameterAst functionHeader.parameters)	

let logJavaMethodAst = function
	  | JavaMain(methodBlock) -> 
			logStringToJavaAst "Java main"; 
			logJavaBlockAst methodBlock
	  | JavaDefaultConstructor(className, constructorBody) -> 
			logListToJavaAst ["Constuctor for Java class"; className];
			logJavaBlockAst constructorBody
		| JavaFunction(methodHeader, methodBlock) -> 
			logJavaFunctionHeaderAst methodHeader;
			logJavaBlockAst methodBlock

let logJavaClassAst = function
	  JavaClass(className, superClassName, javaMethodList) -> logListToJavaAst ["Java class "; className; "extending"; superClassName; "with"; string_of_int (List.length javaMethodList); "methods"]; 
		ignore (List.map logJavaMethodAst javaMethodList)
	
let logJavaClassListAst = function
    JavaClassList(javaClassList) -> logListToJavaAst ["Java class list of size"; string_of_int (List.length javaClassList)]; ignore (List.map logJavaClassAst javaClassList)