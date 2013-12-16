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
	| _ -> false

let canApplyMinus = function
	| [NumberType(numberType1); NumberType(numberType2)] -> (numberType1 = numberType2)
	| _ -> false

(* TODO need to make this work for rings *)
let canApplyTimes = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| _ -> false

(* TODO need to make this work for fields *)
let canApplyDivide = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| _ -> false

let canApplyDivide = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| _ -> false

let canApplyMod = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
	| _ -> false

let canApplyRaise = function
	| [NumberType("Integers"); NumberType("Integers")] -> true
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
		 | _ -> raise(operatorException Plus inputTypeList)) 
	| Minus -> 
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
		 | _ -> raise(operatorException Minus inputTypeList)) 
	| Times -> 
		(match inputTypeList with
		 | [NumberType(groupName); _] -> groupName
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
	| SetLiteral(setopExpression) ->
		(let setExpression =  checkExpression environment setopExpression
		 in let expressionType = getExpressionType setExpression
		 	in TypedSet(expressionType, setExpression))

let rec checkStatement environment = function
	| Print(expression) -> TypedPrint(checkExpression environment expression)
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

let checkProgram = function
	  Program(mainBlock) -> TypedProgram(checkMainBlock mainBlock)	 

(* Convert Sast to Java Ast *)
let createJavaType = function
	| BooleanType -> JavaBooleanType
	| NumberType(_) -> JavaIntType

let rec createJavaExpression = function
	  TypedBoolean(booleanValue, _) -> JavaBoolean(booleanValue)
	| TypedNumber(numberValue, _)-> JavaInt(numberValue)
  | TypedIdentifier(variableName, _) -> JavaVariable(variableName)
	| TypedUnop(unaryOperator, operatorType, unopExpression) ->
		JavaCall(getOperatorCallClass [getExpressionType unopExpression] unaryOperator, operatorToString unaryOperator, [createJavaExpression unopExpression])
	| TypedBinop(binaryOperator, operatorType, binaryExpression1, binaryExpression2) ->
		JavaCall(getOperatorCallClass [getExpressionType binaryExpression1; getExpressionType binaryExpression2] binaryOperator, operatorToString binaryOperator, [createJavaExpression binaryExpression1; createJavaExpression binaryExpression2])
	| TypedSet(setType, setExpression) ->
		JavaCall("SetLiterals", "newHashSet", [createJavaExpression setExpression])

let createJavaStatement = function
	  TypedPrint(expression) -> JavaStatement(JavaCall("System.out", "println", [createJavaExpression expression]))
	| TypedAssignment((variableName, variableType), newValue) -> JavaStatement(JavaAssignment(variableName, createJavaExpression newValue))
	| TypedDeclaration((variableName, variableType), newValue) -> JavaStatement(JavaDeclaration(createJavaType variableType, variableName, Some(createJavaExpression newValue)))

let createJavaMain = function
	  TypedStatementBlock(statementList) -> JavaMain(JavaBlock(List.map createJavaStatement statementList))

let createJavaClass = function 
	  TypedMainBlock(typedStatementList) -> [JavaClass("Main", [createJavaMain typedStatementList])]

let createJavaAst = function
	  TypedProgram(typedMainBlock) -> JavaClassList(createJavaClass typedMainBlock)
		
(* Generate code from Java Ast *)		
let generateJavaType logToJavaFile = function
	| JavaBooleanType -> logToJavaFile "boolean "
	| JavaIntType -> logToJavaFile "int "

let rec generateJavaExpression logToJavaFile = function
	  JavaBoolean(booleanValue) -> logToJavaFile (string_of_bool booleanValue)
	| JavaInt(intValue) -> logToJavaFile (string_of_int intValue)
	| JavaVariable(stringValue) -> logToJavaFile stringValue
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

let generateJavaMethod logToJavaFile = function
	  JavaMain(javaBlock) -> 
			logToJavaFile "public static void main(String[] args) "; 
			generateJavaBlock logToJavaFile javaBlock

let generateJavaClass fileName = function
	  JavaClass(javaClassName, javaMethodList) -> 
			let fullClassName = String.concat "_" [javaClassName; fileName]
			in let logToJavaFile = logToFileAppend false (String.concat "" [fullClassName; ".java"])
				 in logToJavaFile (String.concat " " ["public class"; fullClassName; "{\n"]);  
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

let generatePlatoSetClass = 
	let logToIntegerClassFile = logToFileOverwrite false "SetLiterals.java"
	in logToIntegerClassFile setLiteralsClassString
		
let generatePlatoClasses = 
	generatePlatoCommonClass;
	generatePlatoBooleanClass;
	generatePlatoIntegerClass;
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
	 
