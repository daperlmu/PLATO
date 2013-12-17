open Ast;;
open Logger;;
open Sast;;
open JavaAst;;
open PlatoLibraryStrings;;
open Filename;;

exception PlatoError of string

let undefinedVariableException variableName =
	PlatoError("Undeclared identifier " ^ variableName)

let castException expressionType  variableType = 
	PlatoError("Cannot cast from " ^ (typeToString expressionType) ^ " to " ^ (typeToString variableType))
		
let operatorException operator inputTypeList = 
	PlatoError("Cannot apply " ^ (operatorToString operator) ^ " to type " ^ (String.concat ", " (List.map typeToString inputTypeList)))

let identityException groupName =
	PlatoError("Error while generating group" ^ groupName ^ ".  Could not find identity element")
		
let inverseException groupName element=
	PlatoError("Error while generating group" ^ groupName ^ ".  Could not find inverse of " ^ (string_of_int element))

(* Intepreter for simple statements *)
let evaluateSimpleUnop unopValue = function
	| Negation -> -unopValue
	| _ -> raise(PlatoError("Invalid simple unop"))

let evaluateSimpleBinop binopValue1 binopValue2 = function
  | Plus -> binopValue1 + binopValue2
	| Minus -> binopValue1 - binopValue2
	| Times -> binopValue1 * binopValue2
	| Divide -> binopValue1 / binopValue2
	| Mod -> binopValue1 mod binopValue2
	| Raise -> int_of_float ((float_of_int binopValue1) ** (float_of_int binopValue2))
	| _ -> raise(PlatoError("Invalid simple binop"))

let rec evaluateSimpleExpression identifierName1 identifierName2 input1 input2 = function
	| Number(numberValue) -> numberValue
	| Identifier(variableName) ->
		(match variableName with
	   | identifierName1 -> input1
		 | identifierName2 -> input2
		 | _ -> raise(undefinedVariableException variableName))	
	| Unop(unaryOperator, unopExpression) -> evaluateSimpleUnop (evaluateSimpleExpression identifierName1 identifierName2 input1 input2 unopExpression) unaryOperator
	| Binop(binaryOperator, binopExpression1, binopExpression2) -> evaluateSimpleBinop (evaluateSimpleExpression identifierName1 identifierName2 input1 input2 binopExpression1) (evaluateSimpleExpression identifierName1 identifierName2 input1 input2 binopExpression2) binaryOperator
	| _ -> raise(PlatoError("Expressions in group add or multiply can only contain basic arithmetic operators"))

(* TODO *)
let evaluateSimpleStatement identifierName1 identifierName2 input1 input2 = function
	| _ -> 0
	| _ -> raise(PlatoError("Statements in group add or multiply can only be returns"))

let evaluateSimpleBinaryFunction input1 input2 = function
	| FunctionDeclaration({ returnType = OtherType(NumberType("Integers")); functionName = binaryFunctionName; parameters = [Parameter(NumberType("Integers"), identifierName1); Parameter(NumberType("Integers"), identifierName2)]}, functionBody) -> 
		evaluateSimpleStatement identifierName1 identifierName2 input1 input2 functionBody
	| _ -> raise(PlatoError("Functions in groups can only be add or multiply"))

(* Convert Ast to Sast *)
let canCast fromType toType = 
  if fromType = toType
	then true
	else false

type symbolTable = {
	(* TODO this would be faster with a set *)
  mutable variables : variableDeclaration list;
}

type translationEnvironemnt = {
	scope : symbolTable;
}

let rec findVariable scope variableName =
  let finderFunction = (function (name, _) -> name = variableName)
	in List.find finderFunction scope.variables

let updateScope scope variableDeclaration = 
	let (variableName, _) = variableDeclaration
	in try ignore (findVariable scope variableName)
	   with Not_found -> 
			scope.variables <- variableDeclaration :: scope.variables 

let emptyEnviroment = 
	let emptyScope = { variables = [] }
	in { scope = emptyScope }

let getExpressionType = function 
	| TypedBoolean(_, expressionType) -> expressionType
	| TypedNumber(_, expressionType) -> expressionType
  | TypedIdentifier(_, expressionType) -> expressionType
	| TypedUnop(_, expressionType, _) -> expressionType
	| TypedBinop(_, expressionType, _, _) -> expressionType
	| TypedSet(expressionType, _) -> expressionType

let canApplyNot = function
	| [BooleanType] -> true
	| _ -> false

let canApplyOr = function
	| [BooleanType; BooleanType] -> true
	| _ -> false

let canApplyAnd = function
	| [BooleanType; BooleanType] -> true
	| _ -> false

let canApplyNegation = function
	| [NumberType(_)] -> true
	| _ -> false

let canApplyNegation = function
	| [NumberType(_)] -> true
	| _ -> false

let canApplyPlus = function
	| [NumberType(numberType1); NumberType(numberType2)] -> (numberType1 = numberType2)
	| [SetLiteralType(arg1); SetLiteralType(arg2)] -> arg1=arg2
	| _ -> false

let canApplyMinus = function
	| [NumberType(numberType1); NumberType(numberType2)] -> (numberType1 = numberType2)
	| _ -> false

(* TODO need to make this work for rings *)
let canApplyTimes = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| [SetLiteralType(arg1); SetLiteralType(arg2)] -> arg1=arg2
	| _ -> false

(* TODO need to make this work for fields *)
let canApplyDivide = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| _ -> false

let canApplyMod = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| _ -> false

let canApplyRaise = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| [SetLiteralType(arg1); SetLiteralType(arg2)] -> arg1=arg2
	| _ -> false

let canApplyLessThan = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| _ -> false

let canApplyLessThanOrEqual = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| _ -> false

let canApplyGreaterThan = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| _ -> false

let canApplyGreaterThanOrEqual = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| _ -> false

let canApplyEqual = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| _ -> false

let canApplySetDifference = function
	| [SetLiteralType(arg1); SetLiteralType(arg2)] -> arg1=arg2
	| _ -> false

let canApplyOperator inputTypeList = function
	| Not -> canApplyNot inputTypeList
	| And -> canApplyAnd inputTypeList
	| Or -> canApplyOr inputTypeList
	| Negation -> canApplyNegation inputTypeList
  | Plus -> canApplyPlus inputTypeList
	| Minus -> canApplyMinus inputTypeList
	| Times -> canApplyTimes inputTypeList
	| Divide -> canApplyDivide inputTypeList
	| Mod -> canApplyMod inputTypeList
	| Raise -> canApplyRaise inputTypeList
	| LessThan -> canApplyLessThan inputTypeList
	| LessThanOrEqual -> canApplyLessThanOrEqual inputTypeList
	| GreaterThan -> canApplyGreaterThan inputTypeList
	| GreaterThanOrEqual -> canApplyGreaterThanOrEqual inputTypeList
	| Equal -> canApplyEqual inputTypeList
	| SetDifference -> canApplySetDifference inputTypeList

let getOperatorReturnType inputTypes = function
	| Not -> BooleanType
	| And -> BooleanType
	| Or -> BooleanType
	| Negation -> 
		(match inputTypes with 
		 | [inputType] -> inputType
		 | _ -> raise(PlatoError("Negation must have exactly have one input type")))
  | Plus -> 
	  (match inputTypes with 
		 | [inputType1; inputTyp2] -> inputType1
		 | _ -> raise(PlatoError("Plus must have exactly have two input types")))
	| Minus -> 
	 (match inputTypes with 
	   | [inputType1; inputTyp2] -> inputType1
		 | _ -> raise(PlatoError("Minus must have exactly have two input types")))
	| Times -> 
	  (match inputTypes with  
	 	 | [inputType1; inputTyp2] -> inputType1
		 | _ -> raise(PlatoError("Times must have exactly have two input types")))
	| Divide -> 
  	(match inputTypes with 
		 |[inputType1; inputTyp2] -> inputType1
		 | _ -> raise(PlatoError("Divide must have exactly have two input types")))
	| Mod -> 
    (match inputTypes with 
		 | [inputType1; inputTyp2] -> inputType1
		 | _ -> raise(PlatoError("Mod must have exactly have two input types")))
	| Raise -> 	
		(match inputTypes with 
		 | [inputType1; inputTyp2] -> inputType1
		 | _ -> raise(PlatoError("Raise must have exactly have two input types")))
	| LessThan -> BooleanType
	| LessThanOrEqual -> BooleanType
	| GreaterThan -> BooleanType
	| GreaterThanOrEqual -> BooleanType
	| Equal -> BooleanType
	| SetDifference ->	
		(match inputTypes with 
		 | [inputType1; inputType2] -> inputType1
		 | _ -> raise(PlatoError("Raise must have exactly have two input types")))

let getOperatorCallClass inputTypeList = function
	| Not -> "Booleans"
	| And -> "Booleans"
	| Or -> "Booleans"
	| Negation -> 
		(match inputTypeList with
		 | [NumberType(groupName)] -> groupName
		 | _ -> raise(operatorException Negation inputTypeList)) 
  | Plus -> 
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | [SetLiteralType(_); _] -> "SetLiterals"
		 | _ -> raise(operatorException Plus inputTypeList)) 
	| Minus -> 
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | _ -> raise(operatorException Minus inputTypeList)) 
	| Times -> 
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | [SetLiteralType(_); _] -> "SetLiterals"
		 | _ -> raise(operatorException Times inputTypeList)) 
	| Divide ->
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | _ -> raise(operatorException Divide inputTypeList)) 
	| Mod ->
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | _ -> raise(operatorException Mod inputTypeList)) 
	| Raise ->
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | [SetLiteralType(_); _] -> "SetLiterals"
		 | _ -> raise(operatorException Raise inputTypeList)) 
	| LessThan -> 
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | _ -> raise(operatorException LessThan inputTypeList)) 
	| LessThanOrEqual ->
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | _ -> raise(operatorException LessThanOrEqual inputTypeList)) 
	| GreaterThan ->
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | _ -> raise(operatorException GreaterThan inputTypeList)) 
	| GreaterThanOrEqual ->
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | _ -> raise(operatorException GreaterThanOrEqual inputTypeList)) 
	| Equal ->
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | _ -> raise(operatorException Equal inputTypeList)) 
	| SetDifference ->
		(match inputTypeList with
		 | [SetLiteralType(_); _] -> "SetLiterals"
		 | _ -> raise(operatorException Equal inputTypeList)) 

let rec checkExpression environment = function
	| Boolean(booleanValue) -> TypedBoolean(booleanValue, BooleanType)
	| Number(numberValue) -> TypedNumber(numberValue, NumberType("Integers"))
  | Identifier(variableName) -> 
		  let variableDeclaration = 
				try findVariable environment.scope variableName 
			  with Not_found -> raise (undefinedVariableException variableName)
		  in  let (_, variableType) = variableDeclaration
			    in TypedIdentifier(variableName, variableType)
	| Unop(unaryOperator, unopExpression) ->
		(let unaryExpression =  checkExpression environment unopExpression
		 in let expressionTypeList = [getExpressionType unaryExpression]
		    in if canApplyOperator expressionTypeList unaryOperator
			     then TypedUnop(unaryOperator, getOperatorReturnType expressionTypeList unaryOperator, unaryExpression)
			     else raise(operatorException unaryOperator expressionTypeList))
	| Binop(binaryOperator, binaryExpression1, binaryExpression2) ->
		(let binaryExpression1 = checkExpression environment binaryExpression1
		 and binaryExpression2 = checkExpression environment binaryExpression2
     in let expressionTypeList = [getExpressionType binaryExpression2; getExpressionType binaryExpression2]
		    in if canApplyOperator expressionTypeList binaryOperator
			     then TypedBinop(binaryOperator, getOperatorReturnType expressionTypeList binaryOperator, binaryExpression1, binaryExpression2)
			     else raise(operatorException binaryOperator expressionTypeList))
	| SetLiteral(setopExpressionList) ->
		(let setExpressionList =  List.map (checkExpression environment) setopExpressionList
		 in let expressionTypeList = List.map getExpressionType setExpressionList
		 	in TypedSet(SetLiteralType(List.hd expressionTypeList), setExpressionList))

let rec checkStatement environment = function
	| Print(expression) -> TypedPrint(checkExpression environment expression)
	| Return(expression) -> TypedReturn(checkExpression environment expression)
  | Assignment(variableName, newValue) -> 
		let variableIdentifier = Identifier(variableName) 
		in let variableDetails = checkExpression environment variableIdentifier
			 in let expressionDetails = checkExpression environment newValue
			     in let expressionType, variableType = (getExpressionType expressionDetails), (getExpressionType variableDetails)
						  in if canCast (getExpressionType expressionDetails) (getExpressionType variableDetails)
				         then TypedAssignment((variableName, variableType), expressionDetails) 
						     else raise(castException expressionType variableType)
	| Declaration(variableType, variableName, newValue) ->
		let expressionDetails = checkExpression environment newValue
		   in let expressionType = (getExpressionType expressionDetails)  
			    in if canCast expressionType variableType
			       then (updateScope environment.scope (variableName, variableType);
						       TypedDeclaration((variableName, variableType), expressionDetails))
					   else raise(castException expressionType variableType)

let checkStatementBlock environment = function
	  StatementBlock(statementList) -> TypedStatementBlock(List.map (checkStatement environment) statementList) 

let checkMainBlock = function
	  MainBlock(mainBlock) -> TypedMainBlock(checkStatementBlock emptyEnviroment mainBlock)

let checkFunctionBlock = function
	  FunctionDeclaration(functionHeader, statementBlock) -> TypedFunctionDeclaration(functionHeader, checkStatementBlock emptyEnviroment statementBlock)

let rec generateTableHelper rowElements columnElements tableFunction tableSoFar =
	match rowElements with
	| [] -> List.rev tableSoFar
	| head::tail -> 
		let paritalTableFunction = (fun input2 -> evaluateSimpleBinaryFunction head input2 tableFunction)
		in generateTableHelper tail columnElements tableFunction ((List.map paritalTableFunction columnElements)::tableSoFar)

let generateTable tableElements tableFunction = generateTableHelper tableElements tableElements tableFunction [ [] ]

let rec getGroupIdentityHelper groupName groupElements groupTable currentIndex = 
	match groupTable with
	| [] -> raise(identityException groupName)
	| head::tail ->
		if head = groupElements
		then List.nth groupElements currentIndex
		else getGroupIdentityHelper groupName groupElements tail (currentIndex + 1)
		
let getGroupIdentity groupName groupElements groupTable = getGroupIdentityHelper groupName groupElements groupTable 0

let rec findInverseIndexHelper groupName element groupIdentity currentIndex = function
	| [] -> raise(inverseException groupName element)
	| head::tail ->
		if head = groupIdentity
		then currentIndex
		else findInverseIndexHelper groupName element groupIdentity (currentIndex + 1) tail

let findInverseIndex groupName element groupIdentity additionResults = findInverseIndexHelper groupName element groupIdentity 0 additionResults

let rec generateInverseTableHelper groupName groupElements groupIdentity groupTable tableSoFar =
	match groupElements with 
	| [] -> List.rev tableSoFar
	| head::tail -> 
		let headInverse = List.nth groupElements (findInverseIndex groupName head groupIdentity (List.hd groupTable))
		in generateInverseTableHelper groupName tail groupIdentity (List.tl groupTable) (headInverse :: tableSoFar)

let generateInverseTable groupName groupElements groupTable = generateInverseTableHelper groupName groupElements (getGroupIdentity groupName groupElements groupTable) groupTable []

(* TODO actually check for associativity using Light's associtivity test *)
let isAssociative groupTable = true

let checkGroupBlock = function
    GroupDeclaration(GroupHeader(groupName), GroupBody(groupElements, groupAdditionFunction)) ->  
			let additionTable = generateTable groupElements groupAdditionFunction
		  in let additiveInverseList = generateInverseTable groupName groupElements additionTable
				 in if isAssociative additionTable
			      then TypedGroupDeclaration(groupName, groupElements, additionTable, additiveInverseList)
				    else raise(PlatoError("Group addition must be associative"))
			   

let checkProgram = function
	  Program(mainBlock, functionBlockList, groupBlockList) -> TypedProgram(checkMainBlock mainBlock, List.map checkFunctionBlock functionBlockList, List.map checkGroupBlock groupBlockList)	 

(* Convert Sast to Java Ast *)
let createJavaType = function
	| BooleanType -> JavaBooleanType
	| NumberType(_) -> JavaIntType
	| SetLiteralType(_) -> JavaSetLiteralType

let rec createJavaExpression = function
	| TypedBoolean(booleanValue, _) -> JavaConstant(JavaValue(JavaBoolean(booleanValue)))
	| TypedNumber(numberValue, _)-> JavaConstant(JavaValue(JavaInt(numberValue)))
  | TypedIdentifier(variableName, _) -> JavaVariable(variableName)
	| TypedUnop(unaryOperator, operatorType, unopExpression) ->
		JavaCall(getOperatorCallClass [getExpressionType unopExpression] unaryOperator, operatorToString unaryOperator, [createJavaExpression unopExpression])
	| TypedBinop(binaryOperator, operatorType, binaryExpression1, binaryExpression2) ->
		JavaCall(getOperatorCallClass [getExpressionType binaryExpression1; getExpressionType binaryExpression2] binaryOperator, operatorToString binaryOperator, [createJavaExpression binaryExpression1; createJavaExpression binaryExpression2])
	| TypedSet(setType, setExpressionList) ->
		JavaCall("SetLiterals", "newPlatoSet", List.map createJavaExpression setExpressionList)

let createJavaStatement = function
	| TypedPrint(expression) -> JavaStatement(JavaCall("System.out", "println", [createJavaExpression expression]))
	| TypedReturn(expression) -> JavaStatement(JavaReturn(createJavaExpression expression))
	| TypedAssignment((variableName, variableType), newValue) -> JavaStatement(JavaAssignment(variableName, createJavaExpression newValue))
	| TypedDeclaration((variableName, variableType), newValue) -> JavaStatement(JavaDeclaration(createJavaType variableType, variableName, Some(createJavaExpression newValue)))

let createJavaFunction = function
	  TypedFunctionDeclaration(functionHeader, TypedStatementBlock(typedStatementList)) -> 
	  		JavaFunction(functionHeader, JavaBlock(List.map createJavaStatement typedStatementList))

let createJavaMain = function
	  TypedStatementBlock(statementList) -> JavaMain(JavaBlock(List.map createJavaStatement statementList))

let createJavaMainClass typedFunctionBlockList = function 
	| TypedMainBlock(typedStatementList) -> JavaClass("Main", "", [], (createJavaMain typedStatementList)::(List.map createJavaFunction typedFunctionBlockList))

let listPairToMap keyList valueList = 
		JavaMap(
			List.map string_of_int keyList, 
			List.map string_of_int valueList)

let listTablePairToMap keyList valueTable = 
	JavaMap(
		List.concat (List.map (fun element1 -> List.map (fun element2 -> string_of_int element1 ^ "," ^ string_of_int element2) keyList) keyList),
		List.map string_of_int (List.concat valueTable))

let createJavaGroupClass = function
	| TypedGroupDeclaration(groupName, groupElements, additionTable, additiveInverseList) -> 
		 JavaClass(
			groupName, 
			"Groups", 
			[JavaInstanceVariable(
				"additionTable", 
				"public static", 
				listTablePairToMap groupElements additionTable);
			 JavaInstanceVariable(
				"additiveInverseList", 
				"public static", 
				listPairToMap groupElements additiveInverseList)],
			[])

let createJavaAst = function
	  (* TODO create the group classes here *)
	  TypedProgram(typedMainBlock, typedFunctionBlockList, typedGroupBlockList) -> JavaClassList((createJavaMainClass typedFunctionBlockList typedMainBlock)::(List.map createJavaGroupClass typedGroupBlockList))
		
(* Generate code from Java Ast *)		
let generateJavaType logToJavaFile = function
	| JavaBooleanType -> logToJavaFile "boolean "
	| JavaIntType -> logToJavaFile "int "
	| JavaSetLiteralType -> logToJavaFile "HashSet<Object> "

let generateJavaPrimitive logToJavaFile = function
	| JavaBoolean(booleanValue) -> logToJavaFile (string_of_bool booleanValue)
	| JavaInt(intValue) -> logToJavaFile (string_of_int intValue)

let rec generatePuts logToJavaFile keyList valueList =
	(if List.length keyList > 0
	then (logToJavaFile ("aMap.put(\"" ^ List.hd keyList ^ "\", \"" ^ List.hd valueList ^ "\");");
	     generatePuts logToJavaFile (List.tl keyList) (List.tl valueList))
	else ())

let generateJavaValue logToJavaFile = function
	| JavaValue(javaPrimitive) -> generateJavaPrimitive logToJavaFile javaPrimitive
	| JavaMap(keyList, valueList) -> 
		logToJavaFile "static {\n Map<String, String> aMap = new HashMap<String, String>();";
		generatePuts logToJavaFile keyList valueList;
		logToJavaFile "myMap = Collections.unmodifiableMap(aMap);\n }\n"

let rec generateJavaExpression logToJavaFile = function
	| JavaConstant(javaValue) -> generateJavaValue logToJavaFile javaValue
	| JavaVariable(stringValue) -> logToJavaFile stringValue
	| JavaReturn(expressionToReturn) ->
		logToJavaFile "return ";
		generateJavaExpression logToJavaFile expressionToReturn
	| JavaAssignment(variableName, variableValue) -> 
		logToJavaFile (variableName ^ "=");
		generateJavaExpression logToJavaFile variableValue
	| JavaDeclaration(variableType, variableName, variableValue) ->
	  (generateJavaType logToJavaFile variableType;
		logToJavaFile (variableName ^ "=");
		match variableValue with
		  | Some(javaExpressionValue) -> generateJavaExpression logToJavaFile javaExpressionValue
		  | None -> () (* do nothing *))
	| JavaCall(className, methodName, javaExpressionList) ->
			logToJavaFile (String.concat "" [className; "."; methodName; "("]);
			(match javaExpressionList with
				[] -> ()
				| [first] -> ignore (generateJavaExpression logToJavaFile first)
				| first::rest -> ignore (generateJavaExpression logToJavaFile first);
				   ignore (List.map (fun elem -> logToJavaFile ","; generateJavaExpression logToJavaFile elem) rest));
			logToJavaFile ")"

let generateJavaStatement logToJavaFile = function
	  JavaStatement(javaExpression) ->
			generateJavaExpression logToJavaFile javaExpression;
			logToJavaFile ";\n"

let generateJavaBlock logToJavaFile = function
	  JavaBlock(javaStatementList) ->
			logToJavaFile "{\n"; 
			ignore (List.map (generateJavaStatement logToJavaFile) javaStatementList);
			logToJavaFile "}\n"

let generateJavaFunctionParameter logToJavaFile = function
	  Parameter(paramType, paramName) ->
			generateJavaType logToJavaFile (createJavaType paramType);
			logToJavaFile paramName

let generateJavaMethod logToJavaFile = function
	  | JavaMain(javaBlock) -> 
			(logToJavaFile "public static void main(String[] args) "; 
			generateJavaBlock logToJavaFile javaBlock)
	  | JavaFunction(functionHeader, javaBlock) ->
	  		(logToJavaFile "public static ";
	  		(match functionHeader.returnType with
				VoidType -> ignore (logToJavaFile "void ")
				| OtherType(returnType) -> ignore (generateJavaType logToJavaFile (createJavaType returnType));
				);
	  		logToJavaFile functionHeader.functionName;
	  		logToJavaFile "(";
	  		(match functionHeader.parameters with
				[] -> ()
				| [first] -> ignore (generateJavaFunctionParameter logToJavaFile first)
				| first::rest -> ignore (generateJavaFunctionParameter logToJavaFile first);
				   ignore (List.map (fun elem -> logToJavaFile ","; generateJavaFunctionParameter logToJavaFile elem) rest));
	  		logToJavaFile ") ";
			generateJavaBlock logToJavaFile javaBlock)

let generateJavaInstanceVariable logToJavaFile = function
	| JavaInstanceVariable(variableName, variableModifiers, variableValue) ->
		generateJavaValue logToJavaFile variableValue;
		logToJavaFile (variableModifiers ^ " " ^ variableName ^ ";")

let generateJavaClass fileName = function
	  JavaClass(javaClassName, javaSuperClassName, javaInstanceVariableList, javaMethodList) -> 
			let fullClassName = String.concat "_" [javaClassName; fileName]
			in let logToJavaFile = logToFileAppend false (String.concat "" [fullClassName; ".java"])
				 in let extendsString = ""
					  in logToJavaFile (String.concat " " ["public class"; fullClassName; extendsString; "{\n"]);  
						    ignore (List.map (generateJavaInstanceVariable logToJavaFile) javaInstanceVariableList);
				       ignore (List.map (generateJavaMethod logToJavaFile) javaMethodList);
			         logToJavaFile "}\n"
			
let generatePlatoCommonClass = 
	let logToCommonClassFile = logToFileOverwrite false "PlatoCommon.java"
	in logToCommonClassFile commonClassString 			
			
let generatePlatoBooleanClass = 
	let logToBooleanClassFile = logToFileOverwrite false "Booleans.java"
	in logToBooleanClassFile booleanClassString
	
let generatePlatoIntegerClass = 
	let logToIntegerClassFile = logToFileOverwrite false "Integers.java"
	in logToIntegerClassFile integerClassString

let generatePlatoSetLiteralsClass = 
	let logToIntegerClassFile = logToFileOverwrite false "SetLiterals.java"
	in logToIntegerClassFile setLiteralsClassString

let generatePlatoSetClass = 
	let logToIntegerClassFile = logToFileOverwrite false "PlatoSet.java"
	in logToIntegerClassFile platoSetClassString

let generatePlatoClasses = 
	generatePlatoCommonClass;
	generatePlatoBooleanClass;
	generatePlatoIntegerClass;
	generatePlatoSetLiteralsClass;
	generatePlatoSetClass		
			
let generateJavaCode fileName = function
	  JavaClassList(javaClassList) -> 
			generatePlatoClasses;
			ignore (List.map (generateJavaClass fileName) javaClassList)

(* TODO: Need to check for correct file extension and existance and permissions for file *)
let compile fileName =
  let lexbuf = Lexing.from_channel (open_in fileName) 
	in let programAst = Parser.program Scanner.token lexbuf
	   in logProgramAst programAst; 
		    let programSast = checkProgram programAst
		    in logProgramSast programSast; 
				   let javaClassListAst = createJavaAst programSast
		       in logJavaClassListAst javaClassListAst; 
					    generateJavaCode (Filename.chop_extension (Filename.basename fileName)) javaClassListAst

let _ = compile Sys.argv.(1)
	 
