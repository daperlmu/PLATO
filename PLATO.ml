open Ast;;
open Logger;;
open Sast;;
open JavaAst;;
open PlatoLibraryStrings;;
open Filename;;

exception PlatoError of string
exception DebugException of string

let allTrue list = List.fold_left (&&) true list

let undeclaredVariableException variableName = PlatoError("Undeclared identifier " ^ variableName)
	
let redeclaredVariableException variableName =
	PlatoError("Identifier " ^ variableName ^ " is already declared")	

let castException expressionType  variableType = 
	PlatoError("Cannot cast from " ^ (typeToString expressionType) ^ " to " ^ (typeToString variableType))
		
let operatorException operator inputTypeList = 
	PlatoError("Cannot apply " ^ (operatorToString operator) ^ " to types " ^ (String.concat ", " (List.map typeToString inputTypeList)))

let identityException groupName =
	PlatoError("Error while generating group, ring or field " ^ groupName ^ ".  Could not find identity element")
		
let inverseException groupName element=
	PlatoError("Error while generating group, ring or field " ^ groupName ^ ".  Could not find inverse of " ^ (string_of_int element))

let voidFunctionHasReturnException functionName = PlatoError("Function: " ^ functionName ^ " is a void function. Void functions cannot return an expression")

let missingReturnStmtException functionName functionReturnType = PlatoError("Function: " ^ functionName ^ " is a typed function of type " ^ functionReturnType ^ ". Missing return statement. Expecting return statement of type " ^ functionReturnType ^ ".")

let incompatibleTypesReturnStmt functionName functionReturnType actualReturn  = PlatoError("Return statement incompatible types for the Function: " ^ functionName ^ ". Required: " ^ functionReturnType ^ ". Found: " ^ actualReturn)

let noneBooleanExpressionInElseIf expressionType = PlatoError("elseif statements expect an expression of type boolean. Actual type of expression: " ^ (typeToString expressionType))
let noneBooleanExpressionInIf expressionType = PlatoError("if statements expect an expression of type boolean. Actual type of expression: " ^ (typeToString expressionType))
let unreachableCodeException = PlatoError("Function has unreachable code!")

let functionCallParameterTypeMismatchException functionName = PlatoError("Function call: " ^ functionName ^ " with the list of provided of parameters could not be matched to any existing function.")

let functionNotFoundException functionName = PlatoError("Function: " ^ functionName ^ " NOT_FOUND_EXCEPTION")
let heterogeneousSetLiteralException variableTypes =
	PlatoError("Set has heterogeneous types: "^(String.concat ", " (List.map typeToString variableTypes)))

let heterogeneousVectorLiteralException variableTypes =
	PlatoError("Vector has heterogeneous types: "^(String.concat ", " (List.map typeToString variableTypes)))

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
		(if variableName = identifierName1 then input1
		 else if variableName = identifierName2 then input2
		 else raise(undeclaredVariableException variableName))
	| Unop(unaryOperator, unopExpression) -> evaluateSimpleUnop (evaluateSimpleExpression identifierName1 identifierName2 input1 input2 unopExpression) unaryOperator
	| Binop(binaryOperator, binopExpression1, binopExpression2) -> evaluateSimpleBinop (evaluateSimpleExpression identifierName1 identifierName2 input1 input2 binopExpression1) (evaluateSimpleExpression identifierName1 identifierName2 input1 input2 binopExpression2) binaryOperator
	| _ -> raise(PlatoError("Expressions in groups', rings' or fields' add or multiply can only contain basic arithmetic operators"))

let evaluateSimpleSet = function
	| SetLiteral(expressionList) -> List.map (evaluateSimpleExpression "" "" 0 0) expressionList
	| _ -> raise(PlatoError("A group, ring or field must have a set of elements")) 

let evaluateSimpleStatement identifierName1 identifierName2 input1 input2 = function
	| Return(javaExpression) -> 
		evaluateSimpleExpression identifierName1 identifierName2 input1 input2 javaExpression
	| _ -> raise(PlatoError("Statements in groups', rings' or fields' add or multiply can only be returns"))

let evaluateSimpleBinaryFunction input1 input2 = function
	| FunctionDeclaration({ returnType = OtherType(NumberType("field", "Integers")); functionName = _; parameters = [Parameter(NumberType("field", "Integers"), identifierName1); Parameter(NumberType("field", "Integers"), identifierName2)]}, StatementBlock([javaStatement])) -> 
		evaluateSimpleStatement identifierName1 identifierName2 input1 input2 javaStatement
	| _ -> raise(PlatoError("Functions in groups, rings or fields can only be add or multiply"))

(* Convert Ast to Sast *)
let extractPltTypeFromFuncType = function
	OtherType(pltType) -> pltType
	| _ -> raise(DebugException("If this point was reached, cannot have voidType"))

let canCast fromType toType = 
  if fromType = toType
	then true
	else 
		match toType with
		| NumberType(_, _) -> (fromType = NumberType("field", "Integers"))
		| _ -> false

let canFunctionCast funcType1 funcType2 =
	match funcType1 with
		VoidType -> funcType2=VoidType
		| OtherType(pltType) -> canCast pltType (extractPltTypeFromFuncType funcType2)

type symbolTable = {
  mutable variables : variableDeclaration list;
}

type translationEnvironemnt = {
	scope : symbolTable;
}

type functionsTable = {
	mutable functionsList : functionDeclaration list;
}
type globalEnvironment = {
	globalScope : functionsTable;
}

let rec findVariable scope variableName =
  let finderFunction = (function (name, _) -> name = variableName)
	in List.find finderFunction scope.variables

let updateScope scope variableDeclaration = 
	let (variableName, _) = variableDeclaration
	in try ignore(findVariable scope variableName); raise(redeclaredVariableException(variableName))
	   with Not_found -> 
			scope.variables <- variableDeclaration :: scope.variables

let getFunctionDeclaration scope functionName = List.find (function (name, _, _) -> name = functionName) scope.functionsList

let isFunctionInEnv scope functionName = 
	try (ignore (List.find (function (name, _, _) -> name = functionName) scope.functionsList)); true 
		with Not_found -> false

let updateGlobalScopeWithFunction scope functionName returnType parameterList = 
	try ignore (List.find (function (name, _, _) -> name = functionName) scope.functionsList)
		with Not_found ->
			scope.functionsList <- (functionName, returnType, parameterList) :: scope.functionsList

let emptyEnviroment = 
	let emptyScope = { variables = [] }
	in { scope = emptyScope }

let emptyGlobalEnvironment = 
	let emptyGlobalScope = { functionsList = [] }
	in { globalScope = emptyGlobalScope }

let convertParamToVarDec = function
	Parameter(variableType, variableName) -> (variableName, variableType)

let getExpressionType = function
	| TypedBoolean(_, expressionType) -> OtherType(expressionType)
	| TypedNumber(_, expressionType) -> OtherType(expressionType)
	| TypedIdentifier(_, expressionType) -> OtherType(expressionType)
	| TypedUnop(_, expressionType, _) -> OtherType(expressionType)
	| TypedBinop(_, expressionType, _, _) -> OtherType(expressionType)
	| TypedSet(expressionType, _) -> OtherType(expressionType)
	| TypedFunctionCall(expressionType, _, _) -> expressionType
	| TypedVector(expressionType, _) -> OtherType(expressionType)
	| TypedVectorRange(_, _, _, _) -> OtherType(VectorLiteralType(NumberType("field", "Integers")))
	(*
	| TypedQuantifier(expressionType, _) -> expressionType
	*)

let canApplyAt = function
	| [NumberType("field", "Integers"); VectorLiteralType(NumberType("field", "Integers"))] -> true
	| [VectorLiteralType(NumberType("field", "Integers")); NumberType("field", "Integers")] -> true
	| [VectorLiteralType(NumberType("field", "Integers")); VectorLiteralType(NumberType("field", "Integers"))] -> true
	| _ -> false

let canApplyNot = function
	| [BooleanType] -> true
	| [VectorLiteralType(BooleanType)] -> true
	| _ -> false

let canApplyOr = function
	| [BooleanType; BooleanType] -> true
	| [VectorLiteralType(BooleanType); BooleanType] -> true
	| [BooleanType; VectorLiteralType(BooleanType)] -> true
	| _ -> false

let canApplyAnd = function
	| [BooleanType; BooleanType] -> true
	| [VectorLiteralType(BooleanType); BooleanType] -> true
	| [BooleanType; VectorLiteralType(BooleanType)] -> true
	| _ -> false

let canApplyNegation = function
	| [NumberType(_, _)] -> true	
	| [VectorLiteralType(NumberType(_, _))] -> true
	| _ -> false

let canApplyPlus = function
	| [NumberType(_, numberType1); NumberType(_, numberType2)] -> (numberType1 = numberType2)
	| [SetLiteralType(arg1); SetLiteralType(arg2)] -> (arg1=arg2)
	| [VectorLiteralType(NumberType(_, numberType1)); NumberType(_, numberType2)] -> (numberType1 = numberType2)
	| [NumberType(_, numberType1); VectorLiteralType(NumberType(_, numberType2))] -> (numberType1 = numberType2)
	| _ -> false

let canApplyMinus = function
	| [NumberType(_, numberType1); NumberType(_, numberType2)] -> (numberType1 = numberType2)
	| [VectorLiteralType(NumberType(_, numberType1)); NumberType(_, numberType2)] -> (numberType1 = numberType2)
	| [NumberType(_, numberType1); VectorLiteralType(NumberType(_, numberType2))] -> (numberType1 = numberType2)
	| _ -> false

let canApplyTimes = function
	| [NumberType(extendedGroupType1, numberType1); NumberType(extendedGroupType2, numberType2)] -> 
		(numberType1 = numberType2) && ((extendedGroupType1 = "ring") || (extendedGroupType1 = "field")) && ((extendedGroupType2 = "ring") || (extendedGroupType2 = "field"))
 | [VectorLiteralType(NumberType(extendedGroupType1, numberType1)); NumberType(extendedGroupType2, numberType2)] -> 
		(numberType1 = numberType2) && ((extendedGroupType1 = "ring") || (extendedGroupType1 = "field")) && ((extendedGroupType2 = "ring") || (extendedGroupType2 = "field"))
	| [NumberType(extendedGroupType1, numberType1); VectorLiteralType(NumberType(extendedGroupType2, numberType2))] -> 
		(numberType1 = numberType2) && ((extendedGroupType1 = "ring") || (extendedGroupType1 = "field")) && ((extendedGroupType2 = "ring") || (extendedGroupType2 = "field"))
	| [SetLiteralType(arg1); SetLiteralType(arg2)] -> arg1=arg2
	| _ -> false

let canApplyDivide = function
	| [NumberType(extendedGroupType1, numberType1); NumberType(extendedGroupType2, numberType2)] -> 
		(numberType1 = numberType2) && (extendedGroupType1 = "field") && (extendedGroupType2 = "field")
  | [VectorLiteralType(NumberType(extendedGroupType1, numberType1)); NumberType(extendedGroupType2, numberType2)] -> 
				(numberType1 = numberType2) && (extendedGroupType1 = "field") && (extendedGroupType2 = "field")
	| [NumberType(extendedGroupType1, numberType1); VectorLiteralType(NumberType(extendedGroupType2, numberType2))] -> 
						(numberType1 = numberType2) && (extendedGroupType1 = "field") && (extendedGroupType2 = "field")
	| _ -> false

let canApplyMod = function
	| [NumberType(_, "Integers"); NumberType(_, "Integers")] -> true
	| [VectorLiteralType(NumberType(_, "Integers")); NumberType(_, "Integers")] -> true
	| [NumberType(_, "Integers"); VectorLiteralType(NumberType(_, "Integers"))] -> true
	| _ -> false

let canApplyRaise = function
	| [NumberType(_, "Integers"); NumberType(_, "Integers")] -> true
	| [SetLiteralType(arg1); SetLiteralType(arg2)] -> arg1=arg2
	| [VectorLiteralType(NumberType(_, "Integers")); NumberType(_, "Integers")] -> true
	| [NumberType(_, "Integers"); VectorLiteralType(NumberType(_, "Integers"))] -> true
	| _ -> false

let canApplyLessThan = function
	| [NumberType(_, "Integers"); NumberType(_, "Integers")] -> true
	| [VectorLiteralType(NumberType(_, "Integers")); NumberType(_, "Integers")] -> true
	| [NumberType(_, "Integers"); VectorLiteralType(NumberType(_, "Integers"))] -> true
	| _ -> false

let canApplyLessThanOrEqual = function
	| [NumberType(_, "Integers"); NumberType(_, "Integers")] -> true
	| [VectorLiteralType(NumberType(_, "Integers")); NumberType(_, "Integers")] -> true
	| [NumberType(_, "Integers"); VectorLiteralType(NumberType(_, "Integers"))] -> true
	| _ -> false

let canApplyGreaterThan = function
	| [NumberType(_, "Integers"); NumberType(_, "Integers")] -> true
	| [VectorLiteralType(NumberType(_, "Integers")); NumberType(_, "Integers")] -> true
	| [NumberType(_, "Integers"); VectorLiteralType(NumberType(_, "Integers"))] -> true
	| _ -> false

let canApplyGreaterThanOrEqual = function
	| [NumberType(_, "Integers"); NumberType(_, "Integers")] -> true
	| [VectorLiteralType(NumberType(_, "Integers")); NumberType(_, "Integers")] -> true
	| [NumberType(_, "Integers"); VectorLiteralType(NumberType(_, "Integers"))] -> true
	| _ -> false

let canApplyEqual = function
	| [NumberType(_, "Integers"); NumberType(_, "Integers")] -> true
	| [VectorLiteralType(NumberType(_, "Integers")); NumberType(_, "Integers")] -> true
	| [NumberType(_, "Integers"); VectorLiteralType(NumberType(_, "Integers"))] -> true
	| _ -> false

let canApplySetDifference = function
	| [SetLiteralType(arg1); SetLiteralType(arg2)] -> arg1=arg2
	| _ -> false

let canApplyVectorAccess = function
	| [VectorLiteralType(_); arg2] -> 
		(canCast arg2 (NumberType("field", "Integers"))) || (canCast arg2 (VectorLiteralType(BooleanType)))	
	| _ -> false

let canApplyOperator inputTypeList operatorType =
	try (ignore (List.find (fun p -> p=VoidType) inputTypeList)); false
		with Not_found -> 
			let pltInputTypeList = List.map (fun elem -> match elem with
													OtherType(pltType) -> pltType
													| _ -> raise(DebugException("If this point was reached, cannot have voidType"))) inputTypeList
				in (match operatorType with 
				      | At -> canApplyAt pltInputTypeList 
							| Not -> canApplyNot pltInputTypeList
							| And -> canApplyAnd pltInputTypeList
							| Or -> canApplyOr pltInputTypeList
							| Negation -> canApplyNegation pltInputTypeList
						  | Plus -> canApplyPlus pltInputTypeList
							| Minus -> canApplyMinus pltInputTypeList
							| Times -> canApplyTimes pltInputTypeList
							| Divide -> canApplyDivide pltInputTypeList
							| Mod -> canApplyMod pltInputTypeList
							| Raise -> canApplyRaise pltInputTypeList
							| LessThan -> canApplyLessThan pltInputTypeList
							| LessThanOrEqual -> canApplyLessThanOrEqual pltInputTypeList
							| GreaterThan -> canApplyGreaterThan pltInputTypeList
							| GreaterThanOrEqual -> canApplyGreaterThanOrEqual pltInputTypeList
							| Equal -> canApplyEqual pltInputTypeList
							| SetDifference -> canApplySetDifference pltInputTypeList
							| VectorAccess -> canApplyVectorAccess pltInputTypeList)

let getOperatorReturnType inputTypes operatorType =
	let pltInputTypeList = List.map (fun elem -> match elem with
													OtherType(pltType) -> pltType
													| _ -> raise(DebugException("If this point was reached, cannot have voidType"))) inputTypes
	 in (match operatorType with
		| At ->
			(match pltInputTypeList with 
			 | [VectorLiteralType(NumberType("field", "Integers")); NumberType("field", "Integers")] -> VectorLiteralType(NumberType("field", "Integers"))
			 | [NumberType("field", "Integers"); VectorLiteralType(NumberType("field", "Integers"))] -> VectorLiteralType(NumberType("field", "Integers"))
	     | [VectorLiteralType(NumberType("field", "Integers")); VectorLiteralType(NumberType("field", "Integers"))] -> VectorLiteralType(NumberType("field", "Integers"))
			 | _ -> raise(PlatoError("Less than must have exactly have two input types")))
		| Not ->
			(match pltInputTypeList with 
			 | [BooleanType] -> BooleanType
			 | [VectorLiteralType(BooleanType)] -> VectorLiteralType(BooleanType)
			 | _ -> raise(PlatoError("Not must have exactly have one boolean input type")))
		| And -> 
			(match pltInputTypeList with 
			 | [BooleanType; BooleanType] -> BooleanType
			 | [VectorLiteralType(BooleanType); BooleanType] -> VectorLiteralType(BooleanType)
			 | [BooleanType; VectorLiteralType(BooleanType)] -> VectorLiteralType(BooleanType)
			 | _ -> raise(PlatoError("And must have exactly have two boolean input types")))
		| Or -> 
			(match pltInputTypeList with 
			 | [BooleanType; BooleanType] -> BooleanType
			 | [VectorLiteralType(BooleanType); BooleanType] -> VectorLiteralType(BooleanType)
			 | [BooleanType; VectorLiteralType(BooleanType)] -> VectorLiteralType(BooleanType)
			 | [VectorLiteralType(BooleanType); VectorLiteralType(BooleanType)] -> VectorLiteralType(BooleanType)
			 | _ -> raise(PlatoError("Or must have exactly have two boolean input types")))
		| Negation -> 
			(match pltInputTypeList with 
			 | [inputType] -> inputType
			 | _ -> raise(PlatoError("Negation must have exactly have one input type")))
		| Plus -> 
		  (match pltInputTypeList with 
			 | [VectorLiteralType(inputType1); inputType2] -> VectorLiteralType(inputType1)
			 | [inputType1; VectorLiteralType(inputType2)] -> VectorLiteralType(inputType2)
			 | [inputType1; inputTyp2] -> inputType1
			 | _ -> raise(PlatoError("Plus must have exactly have two input types")))
		| Minus -> 
		 (match pltInputTypeList with 
			 | [VectorLiteralType(inputType1); inputType2] -> VectorLiteralType(inputType1)
			 | [inputType1; VectorLiteralType(inputType2)] -> VectorLiteralType(inputType2)
			 | [inputType1; inputTyp2] -> inputType1
			 | _ -> raise(PlatoError("Minus must have exactly have two input types")))
		| Times -> 
		  (match pltInputTypeList with  
			 | [VectorLiteralType(inputType1); inputType2] -> VectorLiteralType(inputType1)
			 | [inputType1; VectorLiteralType(inputType2)] -> VectorLiteralType(inputType2)
			 | [inputType1; inputTyp2] -> inputType1
			 | _ -> raise(PlatoError("Times must have exactly have two input types")))
		| Divide -> 
	  	(match pltInputTypeList with 
			 | [VectorLiteralType(inputType1); inputType2] -> VectorLiteralType(inputType1)
			 | [inputType1; VectorLiteralType(inputType2)] -> VectorLiteralType(inputType2)
			 | [inputType1; inputTyp2] -> inputType1
			 | _ -> raise(PlatoError("Divide must have exactly have two input types")))
		| Mod -> 
	    (match pltInputTypeList with 
			 | [VectorLiteralType(inputType1); inputType2] -> VectorLiteralType(inputType1)
			 | [inputType1; VectorLiteralType(inputType2)] -> VectorLiteralType(inputType2)
			 | [inputType1; inputTyp2] -> inputType1
			 | _ -> raise(PlatoError("Mod must have exactly have two input types")))
		| Raise -> 	
			(match pltInputTypeList with 
			 | [VectorLiteralType(inputType1); inputType2] -> VectorLiteralType(inputType1)
			 | [inputType1; VectorLiteralType(inputType2)] -> VectorLiteralType(inputType2)
			 | [inputType1; inputTyp2] -> inputType1
			 | _ -> raise(PlatoError("Raise must have exactly have two input types")))
		| LessThan -> 
			(match pltInputTypeList with 
			 | [VectorLiteralType(NumberType("field", "Integers")); NumberType("field", "Integers")] -> VectorLiteralType(BooleanType)
			 | [NumberType("field", "Integers"); NumberType("field", "Integers")] -> BooleanType
			 | _ -> raise(PlatoError("Less than must have exactly have two input types")))
		| LessThanOrEqual -> 
		  (match pltInputTypeList with 
			 | [VectorLiteralType(NumberType("field", "Integers")); NumberType("field", "Integers")] -> VectorLiteralType(BooleanType)
			 | [NumberType("field", "Integers"); NumberType("field", "Integers")] -> BooleanType
			 | _ -> raise(PlatoError("Less than must have exactly have two input types")))
		| GreaterThan -> 
			(match pltInputTypeList with 
			 | [VectorLiteralType(NumberType("field", "Integers")); NumberType("field", "Integers")] -> VectorLiteralType(BooleanType)
			 | [NumberType("field", "Integers"); NumberType("field", "Integers")] -> BooleanType
			 | _ -> raise(PlatoError("Less than must have exactly have two input types")))
		| GreaterThanOrEqual -> 
			(match pltInputTypeList with 
			 | [VectorLiteralType(NumberType("field", "Integers")); NumberType("field", "Integers")] -> VectorLiteralType(BooleanType)
			 | [NumberType("field", "Integers"); NumberType("field", "Integers")] -> BooleanType
			 | _ -> raise(PlatoError("Less than must have exactly have two input types")))
		| Equal -> 
			(match pltInputTypeList with 
			 | [VectorLiteralType(NumberType("field", "Integers")); NumberType("field", "Integers")] -> VectorLiteralType(BooleanType)
			 | [NumberType("field", "Integers"); NumberType("field", "Integers")] -> BooleanType
			 | _ -> raise(PlatoError("Less than must have exactly have two input types")))
		| SetDifference ->	
			(match pltInputTypeList with 
			 | [inputType1; inputType2] -> inputType1
			 | _ -> raise(PlatoError("Set difference must have exactly have two input types")))
		| VectorAccess ->
			(match pltInputTypeList with 
			 | [VectorLiteralType(arg1); arg2] -> 
				(if (canCast arg2 (NumberType("field", "Integers")))
				 then arg1
				 else VectorLiteralType(arg1))
			 | _ -> raise(PlatoError("VectorAccess must have exactly have two input types, and the first type must be a VectorLiteral"))) 

		)

let getOperatorCallClass inputTypeList = function
	| At -> "VectorLiterals"
	| Not -> "Booleans"
	| And -> "Booleans"
	| Or -> "Booleans"
	| Negation -> 
		(match inputTypeList with
		 | [NumberType(_, groupName)] -> groupName
		 | [VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | _ -> raise(operatorException Negation inputTypeList)) 
  | Plus -> 
		(match inputTypeList with
		 | [VectorLiteralType(NumberType(_, groupName)); NumberType(_, _)] -> groupName
		 | [NumberType(_, _); VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | [NumberType(_, groupName); _] -> groupName
		 | [SetLiteralType(_); _] -> "SetLiterals"
		 | _ -> raise(operatorException Plus inputTypeList)) 
	| Minus -> 
		(match inputTypeList with
		 | [VectorLiteralType(NumberType(_, groupName)); NumberType(_, _)] -> groupName
		 | [NumberType(_, _); VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | [NumberType(_, groupName); _] -> groupName
		 | _ -> raise(operatorException Minus inputTypeList)) 
	| Times -> 
		(match inputTypeList with
		 | [VectorLiteralType(NumberType(_, groupName)); NumberType(_, _)] -> groupName
		 | [NumberType(_, _); VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | [NumberType(_, groupName); _] -> groupName
		 | [SetLiteralType(_); _] -> "SetLiterals"
		 | _ -> raise(operatorException Times inputTypeList)) 
	| Divide ->
		(match inputTypeList with
		 | [VectorLiteralType(NumberType(_, groupName)); NumberType(_, _)] -> groupName
		 | [NumberType(_, _); VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | [NumberType(_, groupName); _] -> groupName
		 | _ -> raise(operatorException Divide inputTypeList)) 
	| Mod ->
		(match inputTypeList with
		 | [VectorLiteralType(NumberType(_, groupName)); NumberType(_, _)] -> groupName
		 | [NumberType(_, _); VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | [NumberType(_, groupName); _] -> groupName
		 | _ -> raise(operatorException Mod inputTypeList)) 
	| Raise ->
		(match inputTypeList with
		 | [VectorLiteralType(NumberType(_, groupName)); NumberType(_, _)] -> groupName
		 | [NumberType(_, _); VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | [NumberType(_, groupName); _] -> groupName
		 | [SetLiteralType(_); _] -> "SetLiterals"
		 | _ -> raise(operatorException Raise inputTypeList)) 
	| LessThan -> 
		(match inputTypeList with
		 | [VectorLiteralType(NumberType(_, groupName)); NumberType(_, _)] -> groupName
		 | [NumberType(_, _); VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | [NumberType(_, groupName); _] -> groupName
		 | _ -> raise(operatorException LessThan inputTypeList)) 
	| LessThanOrEqual ->
		(match inputTypeList with
		 | [VectorLiteralType(NumberType(_, groupName)); NumberType(_, _)] -> groupName
		 | [NumberType(_, _); VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | [NumberType(_, groupName); _] -> groupName
		 | _ -> raise(operatorException LessThanOrEqual inputTypeList)) 
	| GreaterThan ->
		(match inputTypeList with
		 | [VectorLiteralType(NumberType(_, groupName)); NumberType(_, _)] -> groupName
		 | [NumberType(_, _); VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | [NumberType(_, groupName); _] -> groupName
		 | _ -> raise(operatorException GreaterThan inputTypeList)) 
	| GreaterThanOrEqual ->
		(match inputTypeList with
		 | [VectorLiteralType(NumberType(_, groupName)); NumberType(_, _)] -> groupName
		 | [NumberType(_, _); VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | [NumberType(_, groupName); _] -> groupName
		 | _ -> raise(operatorException GreaterThanOrEqual inputTypeList)) 
	| Equal ->
		(match inputTypeList with
		 | [VectorLiteralType(NumberType(_, groupName)); NumberType(_, _)] -> groupName
		 | [NumberType(_, _); VectorLiteralType(NumberType(_, groupName))] -> groupName
		 | [NumberType(_, groupName); _] -> groupName
		 | _ -> raise(operatorException Equal inputTypeList)) 
	| SetDifference ->
		(match inputTypeList with
		 | [SetLiteralType(_); _] -> "SetLiterals"
		 | _ -> raise(operatorException SetDifference inputTypeList))
	| VectorAccess ->
		(match inputTypeList with
		 | [VectorLiteralType(_); _] -> ("VectorLiterals")
		 | _ -> raise(operatorException VectorAccess inputTypeList)) 

let rec checkExpression globalEnv environment = function
	| Boolean(booleanValue) -> TypedBoolean(booleanValue, BooleanType)
	| Number(numberValue) -> TypedNumber(numberValue, NumberType("field", "Integers"))
	| Identifier(variableName) -> 
		  let variableDeclaration = 
				try findVariable environment.scope variableName 
			  with Not_found -> raise (undeclaredVariableException variableName)
		  in  let (_, variableType) = variableDeclaration
			    in TypedIdentifier(variableName, variableType)
	| Unop(unaryOperator, unopExpression) ->
		(let unaryExpression =  checkExpression globalEnv environment unopExpression
		 in let expressionTypeList = [getExpressionType unaryExpression]
		    in if canApplyOperator expressionTypeList unaryOperator
			     then TypedUnop(unaryOperator, getOperatorReturnType expressionTypeList unaryOperator, unaryExpression)
			     else 
			     	let pltExpressionTypeList = List.map (fun elem -> match elem with
													OtherType(pltType) -> pltType
													| _ -> raise(DebugException("If this point was reached, cannot have voidType"))) expressionTypeList
			     	in raise(operatorException unaryOperator pltExpressionTypeList))
	| Binop(binaryOperator, binaryExpression1, binaryExpression2) ->
		(let binaryExpression1 = checkExpression globalEnv environment binaryExpression1
		 and binaryExpression2 = checkExpression globalEnv environment binaryExpression2
     	in let expressionTypeList = [getExpressionType binaryExpression1; getExpressionType binaryExpression2]
		    in if canApplyOperator expressionTypeList binaryOperator
			     then TypedBinop(binaryOperator, getOperatorReturnType expressionTypeList binaryOperator, binaryExpression1, binaryExpression2)
			     else
			     	let pltExpressionTypeList = List.map (fun elem -> match elem with
													OtherType(pltType) -> pltType
													| _ -> raise(DebugException("If this point was reached, cannot have voidType"))) expressionTypeList 
			     	   in raise(operatorException binaryOperator pltExpressionTypeList))
	| SetLiteral(setopExpressionList) ->
		(match setopExpressionList with
			[] -> TypedSet(SetLiteralType(NeutralType), [])
			| _ -> (let setExpressionList =  List.map (checkExpression globalEnv environment) setopExpressionList
				 in let expressionTypeList = List.map getExpressionType setExpressionList
				 	in let headExpressionType = List.hd expressionTypeList
				 		in if allTrue (List.map (fun arg1 -> (headExpressionType=arg1)) expressionTypeList)
				 			then TypedSet(SetLiteralType(extractPltTypeFromFuncType (List.hd expressionTypeList)), setExpressionList)
				 			else raise(heterogeneousSetLiteralException (List.map extractPltTypeFromFuncType expressionTypeList) )))
	| VectorLiteral(vectoropExpressionList) ->
		(match vectoropExpressionList with
			[] -> TypedVector(VectorLiteralType(NeutralType), [])
			| _ -> (let vectorExpressionList =  List.map (checkExpression globalEnv environment) vectoropExpressionList
				 in let expressionTypeList = List.map getExpressionType vectorExpressionList
				 	in let headExpressionType = List.hd expressionTypeList
				 		in if allTrue (List.map (fun arg1 -> (headExpressionType=arg1)) expressionTypeList)
				 			 then TypedVector(VectorLiteralType(extractPltTypeFromFuncType (List.hd expressionTypeList)), vectorExpressionList)
				 			 else raise(heterogeneousVectorLiteralException (List.map extractPltTypeFromFuncType expressionTypeList))))
	| VectorRange(fromExpression, toExpression, byExpression) ->
		let fromExpression, toExpression, byExpression = (checkExpression globalEnv environment fromExpression), (checkExpression globalEnv environment toExpression), (checkExpression globalEnv environment byExpression)
	  in let fromExpressionType, toExpressionType, byExpressionType = (getExpressionType fromExpression), (getExpressionType toExpression), (getExpressionType byExpression)
        in if canCast (extractPltTypeFromFuncType fromExpressionType) (NumberType("field", "Integers"))
				   then if canCast (extractPltTypeFromFuncType toExpressionType) (NumberType("field", "Integers"))
						     then if canCast (extractPltTypeFromFuncType byExpressionType) (NumberType("field", "Integers"))
									     then TypedVectorRange(VectorLiteralType(NumberType("field", "Integers")), fromExpression, toExpression, byExpression)
											else raise(castException (extractPltTypeFromFuncType byExpressionType) (NumberType("field", "Integers")))
						     else raise(castException (extractPltTypeFromFuncType byExpressionType) (NumberType("field", "Integers")))
					else raise(castException (extractPltTypeFromFuncType byExpressionType) (NumberType("field", "Integers")))
	| FunctionCall(functionName, expressionList) -> 
		if isFunctionInEnv globalEnv.globalScope functionName
		then
			let (_, functionType, parameterList) = getFunctionDeclaration globalEnv.globalScope functionName
			in let listOfCheckedExpressions = List.map (checkExpression globalEnv environment) expressionList
				in let listOfExpressionTypes = List.map extractPltTypeFromFuncType (List.map getExpressionType listOfCheckedExpressions)
					in let listOfTypesOfParams = List.map (fun elem -> match elem with 
																		Parameter(platoType, _) -> platoType) parameterList
						in if listOfExpressionTypes=listOfTypesOfParams
							then TypedFunctionCall(functionType, functionName, listOfCheckedExpressions)
							else raise(functionCallParameterTypeMismatchException functionName)
		else raise(functionNotFoundException functionName)

let getTypedStmtBlockReturnType = function 
	TypedStatementBlock(typedStatementList) ->
		if (List.length typedStatementList)=0
		then VoidType
		else
		(* everything but last stmt should have VoidType, otherwise exception *)
			((ignore (List.map (fun stmt -> match stmt with
					TypedReturn(returnType, _) -> raise(unreachableCodeException)
					| TypedIf(returnType, _, _, _, _) -> (match returnType with
																VoidType -> ()
																| _ -> raise(unreachableCodeException))
					| _ -> ()) (List.tl (List.rev typedStatementList))));
			let lastStmt = List.hd (List.rev typedStatementList)
			in  ( match lastStmt with
					TypedReturn(returnType, _) -> returnType
					| TypedIf(returnType, _, _, _, _) -> returnType
					| _ -> VoidType ))
let rec checkStatement globalEnv environment = function
	| VoidCall(expression) -> 
		let typedExpression = checkExpression globalEnv environment expression 
		in if (getExpressionType typedExpression) = VoidType
		   then TypedVoidCall(checkExpression globalEnv environment expression)
			 else raise(PlatoError("A bare statement can only contain a void function call"))
	| Print(expression) -> TypedPrint(checkExpression globalEnv environment expression)
	| Return(expression) -> 
		let checkedExpression = checkExpression globalEnv environment expression
		in TypedReturn(getExpressionType checkedExpression, checkedExpression)
	| If (expression, statementBlock, elseIfBlockList, elseBlock) ->
		let typedExpression = checkExpression globalEnv environment expression
		in let expressionType = getExpressionType typedExpression
			in if canCast (extractPltTypeFromFuncType expressionType) BooleanType
				then 
					let checkedStatementBlock = checkStatementBlock globalEnv environment statementBlock
					in let typeOfLastStmtInBlock = getTypedStmtBlockReturnType checkedStatementBlock 
						in let checkedElseIfs = List.map (checkElseIfBlock globalEnv environment) elseIfBlockList
							in let checkedElse = checkElseBlock globalEnv environment elseBlock
								in if typeOfLastStmtInBlock = VoidType 
									then TypedIf(VoidType, typedExpression, checkedStatementBlock, checkedElseIfs, checkedElse)
								else
									let checkedElseTypedStmtBlock = (match checkedElse with
																		TypedElseBlock(typedStatementBlock) -> typedStatementBlock)
										in let checkedElseIfsTypedStmtBlocks = List.map (fun element -> match element with
																					TypedElseIfBlock(_, typedStatementBlock) -> typedStatementBlock
																					) checkedElseIfs
											in let typesOfLastStmtsInElseIfAndElseBlocks = (getTypedStmtBlockReturnType checkedElseTypedStmtBlock)::(List.map getTypedStmtBlockReturnType checkedElseIfsTypedStmtBlocks)
											in if List.fold_left (fun booleanValue expressionType -> booleanValue && (expressionType = typeOfLastStmtInBlock)) true typesOfLastStmtsInElseIfAndElseBlocks
												then TypedIf(typeOfLastStmtInBlock, typedExpression, checkedStatementBlock, checkedElseIfs, checkedElse)
												else TypedIf(VoidType, typedExpression, checkedStatementBlock, checkedElseIfs, checkedElse)
				else raise(noneBooleanExpressionInIf (extractPltTypeFromFuncType expressionType))
	| IfNoElse (expression, statementBlock, elseIfBlockList) -> 
		let typedExpression = checkExpression globalEnv environment expression
		in let expressionType = getExpressionType typedExpression
			in if canCast (extractPltTypeFromFuncType expressionType) BooleanType
				then TypedIfNoElse(typedExpression, checkStatementBlock globalEnv environment statementBlock, List.map (checkElseIfBlock globalEnv environment) elseIfBlockList)
				else raise(noneBooleanExpressionInIf (extractPltTypeFromFuncType expressionType))
  | Assignment(variableName, newValue) -> 
		let variableIdentifier = Identifier(variableName) 
		in let variableDetails = checkExpression globalEnv environment variableIdentifier
			 in let expressionDetails = checkExpression globalEnv environment newValue
			     in let expressionType, variableType = (getExpressionType expressionDetails), (getExpressionType variableDetails)
						  in if canCast (extractPltTypeFromFuncType expressionType) (extractPltTypeFromFuncType variableType)
				             then TypedAssignment((variableName, (extractPltTypeFromFuncType variableType)), expressionDetails) 
						     else raise(castException (extractPltTypeFromFuncType expressionType) (extractPltTypeFromFuncType variableType))
	| VectorAssignment(variableName, variableIndex, newValue) -> 
		let variableIdentifier = Identifier(variableName)
		 in let variableDetails, variableIndexDetails, expressionDetails = (checkExpression globalEnv environment variableIdentifier), (checkExpression globalEnv environment variableIndex), (checkExpression globalEnv environment newValue)
		     in let variableIndexType, expressionType, variableType = (getExpressionType variableIndexDetails), (getExpressionType expressionDetails), (getExpressionType variableDetails)
		 		  in (match (extractPltTypeFromFuncType variableType) with
		 				| VectorLiteralType(variableSubType) -> if ((canCast (extractPltTypeFromFuncType expressionType) variableSubType) 
				 		  	  && (canCast (extractPltTypeFromFuncType variableIndexType) (NumberType("field", "Integers"))))
				             then TypedVectorAssignment((variableName, (extractPltTypeFromFuncType variableType)), variableIndexDetails, expressionDetails) 
				 		     else raise(castException (extractPltTypeFromFuncType expressionType) (extractPltTypeFromFuncType variableType))
				 				| _ -> raise(PlatoError("Cannot use vector assignment for non-vector.")))
	| Declaration(variableType, variableName, newValue) ->
		let expressionDetails = checkExpression globalEnv environment newValue
		   in let expressionType = (getExpressionType expressionDetails)  
			    in if canCast (extractPltTypeFromFuncType expressionType) variableType
			       then (updateScope environment.scope (variableName, variableType);
						       TypedDeclaration((variableName, variableType), expressionDetails))
					   else raise(castException (extractPltTypeFromFuncType expressionType) variableType)
and checkStatementBlock globalEnv environment = function
	  StatementBlock(statementList) -> TypedStatementBlock(List.map (checkStatement globalEnv environment) statementList)
and checkElseIfBlock globalEnv environment = function 
	ElseIfBlock(expression, statementBlock) -> 
		let typedExpression = checkExpression globalEnv environment expression
		in let expressionType = getExpressionType typedExpression
			in if canCast (extractPltTypeFromFuncType expressionType) BooleanType
				then TypedElseIfBlock(typedExpression, checkStatementBlock globalEnv environment statementBlock)
				else raise(noneBooleanExpressionInElseIf (extractPltTypeFromFuncType expressionType))
and checkElseBlock globalEnv environment = function 
	ElseBlock(statementBlock) -> 
		TypedElseBlock(checkStatementBlock globalEnv environment statementBlock)

let checkMainBlock globalEnv = function
	  MainBlock(mainBlock) -> TypedMainBlock(checkStatementBlock globalEnv emptyEnviroment mainBlock)

let rec getReturnStmtsHelper = function 
	[] -> []
	| Return(expression)::[] -> [Return(expression)]
	| Return(expression)::tail -> Return(expression)::getReturnStmtsHelper tail
	| _::tail -> getReturnStmtsHelper tail

let getReturnStmts = function 
	StatementBlock(statementList) -> 
		getReturnStmtsHelper(statementList)

let getLastStmtInBlock = function
	StatementBlock(statementList) -> List.hd (List.rev statementList)

let functionTypeToString = function 
	VoidType -> "void"
	| OtherType(pltType) -> typeToString pltType
let checkFunctionBlock globalEnv = function
	  FunctionDeclaration(functionHeader, statementBlock) -> 
	  	let functionReturnType = functionHeader.returnType
	  		in let functionEnvironment = emptyEnviroment
	  		   in (ignore (List.map (updateScope functionEnvironment.scope) (List.map convertParamToVarDec functionHeader.parameters)));
	  		   		(ignore (updateGlobalScopeWithFunction globalEnv.globalScope functionHeader.functionName functionHeader.returnType functionHeader.parameters));
	  			   let checkedStatementBlock = checkStatementBlock globalEnv functionEnvironment statementBlock
	  			 	in let stmtBlockReturnType = getTypedStmtBlockReturnType checkedStatementBlock
	  					in if canFunctionCast stmtBlockReturnType functionReturnType
	  						then  
	  							TypedFunctionDeclaration(functionHeader, checkedStatementBlock)
	  						else raise(incompatibleTypesReturnStmt functionHeader.functionName (functionTypeToString functionHeader.returnType) (functionTypeToString stmtBlockReturnType))

let rec generateTableHelper rowElements columnElements tableFunction tableSoFar =
	match rowElements with
	| [] -> List.rev tableSoFar
	| head::tail -> 
		let paritalTableFunction = (fun input2 -> evaluateSimpleBinaryFunction head input2 tableFunction)
		in generateTableHelper tail columnElements tableFunction ((List.map paritalTableFunction columnElements)::tableSoFar)

let generateTable tableElements tableFunction = generateTableHelper tableElements tableElements tableFunction []

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

let rec generateInverseListHelper groupName groupElements remainingElements groupIdentity remainingTable listSoFar =
	match remainingElements with 
	| [] -> List.rev listSoFar
	| head::tail ->
		let headInverse = List.nth groupElements (findInverseIndex groupName head groupIdentity (List.hd remainingTable))
		in generateInverseListHelper groupName groupElements tail groupIdentity (List.tl remainingTable) (headInverse :: listSoFar)

let generateInverseList groupName groupElements groupIdentity groupTable = generateInverseListHelper groupName groupElements groupElements groupIdentity groupTable []

let print_table table = 
	ignore (List.map (fun intList -> ignore (List.map print_int intList); print_string "\n") table)

let rec isElement list element =
	match list with 
	| [] -> false
	| head::tail -> if element = head
	                then true
									else isElement tail element

let isClosed groupElements groupTable = allTrue (List.map (fun intList -> allTrue (List.map (isElement groupElements) intList)) groupTable)	
	
let rec getIndexHelper element list startIndex =
	match list with 
	| [] -> raise Not_found
	| head::tail -> 
		if element = head 
	  then startIndex
	  else getIndexHelper element tail (startIndex + 1)

let getIndex element list = getIndexHelper element list 0		

let checkAssociative a b c groupElements groupTable = 
	let groupTimes = fun a b -> List.nth (List.nth groupTable (getIndex a groupElements)) (getIndex b groupElements)
	in let starResult = groupTimes (groupTimes a b) c
	   in let circleResult = groupTimes a (groupTimes b c)
		    in starResult = circleResult
						
let rec checkAssociativeList aList b c groupElements groupTable	=
	match aList with
	| [] -> true
	| head::tail -> if checkAssociative head b c groupElements groupTable 
	                then checkAssociativeList tail b c groupElements groupTable	
		              else false
								
let rec checkAssociativeListPair aList bList c groupElements groupTable =	
	match bList with
	| [] -> true
	| head::tail -> if checkAssociativeList aList head c groupElements groupTable 
	                then checkAssociativeListPair aList tail c groupElements groupTable	
		              else false		
		
let rec checkAssociativeListTriple aList bList cList groupElements groupTable =	
	match cList with
	| [] -> true
	| head::tail -> if checkAssociativeListPair aList bList head groupElements groupTable 
	                then checkAssociativeListTriple aList bList tail groupElements groupTable	
		              else false				
					
let isAssociative groupElements groupTable = checkAssociativeListTriple groupElements groupElements groupElements groupElements groupTable 														
						
let rec removeNthHelper n list acc = 
	if n = 0 
	then (List.rev acc) @ (List.tl list)
	else removeNthHelper (n - 1) (List.tl list) ((List.hd list)::acc)					
						
let removeNth n list = removeNthHelper n list []						
						
let rec isCommutative table =
	match table with 
	| [] -> true
	| _ -> if List.hd table = List.map List.hd table
	       then isCommutative (List.tl (List.map List.tl table))
				 else false						

let checkDistributive a b c groupElements additionTable multiplicationTable = 
  let groupPlus = fun a b -> List.nth (List.nth additionTable (getIndex a groupElements)) (getIndex b groupElements)
	in let groupTimes = fun a b -> List.nth (List.nth multiplicationTable (getIndex a groupElements)) (getIndex b groupElements)
	   in let starResult = groupTimes a (groupPlus b c)
	      in let circleResult = groupPlus (groupTimes a b) (groupTimes a c)
		       in starResult = circleResult
						
let rec checkDistributiveList aList b c groupElements additionTable multiplicationTable	=
	match aList with
	| [] -> true
	| head::tail -> if checkDistributive head b c groupElements additionTable multiplicationTable 
	                then checkDistributiveList tail b c groupElements additionTable multiplicationTable	
		              else false
								
let rec checkDistributiveListPair aList bList c groupElements additionTable multiplicationTable =	
	match bList with
	| [] -> true
	| head::tail -> if checkDistributiveList aList head c groupElements additionTable multiplicationTable 
	                then checkDistributiveListPair aList tail c groupElements additionTable multiplicationTable	
		              else false		
		
let rec checkDistributiveListTriple aList bList cList groupElements additionTable multiplicationTable =	
	match cList with
	| [] -> true
	| head::tail -> if checkDistributiveListPair aList bList head groupElements additionTable multiplicationTable 
	                then checkDistributiveListTriple aList bList tail groupElements additionTable multiplicationTable	
		              else false				
					
let distributes groupElements additionTable multiplicationTable = checkDistributiveListTriple groupElements groupElements groupElements groupElements additionTable multiplicationTable
						
let checkExtendedGroupBlock = function
	 | GroupDeclaration(GroupHeader(groupName), GroupBody(groupElements, groupAdditionFunction)) -> 
		  let groupElementList = evaluateSimpleSet groupElements
			in let additionTable = generateTable groupElementList groupAdditionFunction
				 in if (isClosed groupElementList additionTable) && (isAssociative groupElementList additionTable)
				    then let additiveInverseList = generateInverseList groupName groupElementList (getGroupIdentity groupName groupElementList additionTable) additionTable
			  	        in TypedGroupDeclaration(groupName, groupElementList, additionTable, additiveInverseList)
			      else raise(PlatoError("Group addition must be closed and associative"))
   | ExtendedGroupDeclaration(RingHeader(groupName), ExtendedGroupBody(GroupBody(groupElements, groupAdditionFunction), extendedGroupMultiplicationFunction)) ->
		let groupElementList = evaluateSimpleSet groupElements
		in let additionTable = generateTable groupElementList groupAdditionFunction
			 in if (isClosed groupElementList additionTable) && (isAssociative groupElementList additionTable) && (isCommutative additionTable)
		      then let additiveInverseList = generateInverseList groupName groupElementList (getGroupIdentity groupName groupElementList additionTable) additionTable
			         in let multiplicationTable = generateTable groupElementList extendedGroupMultiplicationFunction
					  	    in if (isClosed groupElementList multiplicationTable) && (isAssociative groupElementList multiplicationTable) && (distributes groupElementList additionTable multiplicationTable)
						    	   then TypedRingDeclaration(groupName, groupElementList, additionTable, additiveInverseList, multiplicationTable)
				 	  			   else raise(PlatoError("Ring multiplication must be closed and associative and distribute over addition"))
				  else raise(PlatoError("Ring addition must be closed and associative"))
	 | ExtendedGroupDeclaration(FieldHeader(groupName), ExtendedGroupBody(GroupBody(groupElements, groupAdditionFunction), extendedGroupMultiplicationFunction)) ->  
			let groupElementList = evaluateSimpleSet groupElements
			in let additionTable = generateTable groupElementList groupAdditionFunction
			   in if (isClosed groupElementList additionTable) && (isAssociative groupElementList additionTable) && (isCommutative additionTable)
		        then let additiveIdentity = getGroupIdentity groupName groupElementList additionTable
						     in let additiveInverseList = generateInverseList groupName groupElementList additiveIdentity additionTable
			              in let multiplicationTable = generateTable groupElementList extendedGroupMultiplicationFunction
						           in if (isClosed groupElementList multiplicationTable) && (isAssociative groupElementList multiplicationTable) && (isCommutative multiplicationTable) && (distributes groupElementList additionTable multiplicationTable)
										      then let additiveIdentityIndex = getIndex additiveIdentity groupElementList
											         in let multiplicitiveInverseList = generateInverseList groupName (removeNth additiveIdentityIndex groupElementList)  (getGroupIdentity groupName groupElementList multiplicationTable) (List.map (removeNth additiveIdentityIndex) (removeNth additiveIdentityIndex multiplicationTable))
											    	      in TypedFieldDeclaration(groupName, groupElementList, additionTable, additiveInverseList, multiplicationTable, multiplicitiveInverseList, additiveIdentity)
									        else raise(PlatoError("Field multiplication must be closed, associative, commutative and distribute over addition"))
				    else raise(PlatoError("Field addition must be closed and associative"))
			   
let checkProgram globalEnv = function
	  Program(mainBlock, functionBlockList, extendedGroupBlockList) -> 
	  	let checkedFunctionBlockList = List.map (checkFunctionBlock globalEnv) functionBlockList
	  		in TypedProgram(checkMainBlock globalEnv mainBlock, checkedFunctionBlockList, List.map checkExtendedGroupBlock extendedGroupBlockList)	 

(* Convert Sast to Java Ast *)
let createJavaType = function
	| BooleanType -> JavaBooleanType
	| NumberType(_, _) -> JavaIntType
	| SetLiteralType(_) -> JavaSetLiteralType
	| VectorLiteralType(_) -> JavaVectorLiteralType
	| NeutralType -> JavaNeutralType

let rec createJavaExpression = function
	| TypedBoolean(booleanValue, _) -> JavaConstant(JavaValue(JavaBoolean(booleanValue)))
	| TypedNumber(numberValue, _)-> JavaConstant(JavaValue(JavaInt(numberValue)))
	| TypedIdentifier(variableName, _) -> JavaVariable(variableName)
	| TypedUnop(unaryOperator, operatorType, unopExpression) ->
		JavaCall(getOperatorCallClass [extractPltTypeFromFuncType (getExpressionType unopExpression)] unaryOperator, operatorToString unaryOperator, [createJavaExpression unopExpression])
	| TypedBinop(binaryOperator, operatorType, binaryExpression1, binaryExpression2) ->
		JavaCall(getOperatorCallClass [extractPltTypeFromFuncType (getExpressionType binaryExpression1); extractPltTypeFromFuncType (getExpressionType binaryExpression2)] binaryOperator, operatorToString binaryOperator, [createJavaExpression binaryExpression1; createJavaExpression binaryExpression2])
	| TypedSet(setType, setExpressionList) ->
		JavaCall("SetLiterals", "newPlatoSet", List.map createJavaExpression setExpressionList)
	| TypedFunctionCall(platoFunctionType, functionName, expressionList) -> 
		JavaCall("", functionName, List.map createJavaExpression expressionList)
	| TypedVector(vectorType, vectorExpressionList) ->
		JavaCall("VectorLiterals", "newPlatoVector", List.map createJavaExpression vectorExpressionList)
	| TypedVectorRange(vectorType, fromExpression, toExpression, byExpression) ->
		JavaCall("VectorLiterals", "newPlatoVectorRange", [createJavaExpression fromExpression; createJavaExpression toExpression; createJavaExpression byExpression])

let rec createJavaStatement = function
	| TypedVoidCall(expression) -> JavaStatement(JavaCall("", "", [createJavaExpression expression]))
	| TypedPrint(expression) -> JavaStatement(JavaCall("System.out", "println", [createJavaExpression expression]))
	| TypedReturn(_, expression) -> JavaStatement(JavaReturn(createJavaExpression expression))
	| TypedIf(_, typedExpression, typedStatementBlock, typedElseIfBlockList, typedElseBlock) -> 
					JavaStatement(
						JavaIf(createJavaExpression typedExpression,
								createJavaBlock typedStatementBlock,
								List.map createJavaElseIf typedElseIfBlockList,
								createJavaElse typedElseBlock))
	| TypedIfNoElse(typedExpression, typedStatementBlock, typedElseIfBlockList) -> 
					JavaStatement(
						JavaIfNoElse(createJavaExpression typedExpression,
								createJavaBlock typedStatementBlock,
								List.map createJavaElseIf typedElseIfBlockList
							)
					)
	| TypedAssignment((variableName, variableType), newValue) -> JavaStatement(JavaAssignment(variableName, createJavaExpression newValue))
	| TypedVectorAssignment((variableName, variableType), indexValue, newValue) -> JavaStatement(JavaVectorAssignment(variableName, createJavaExpression indexValue, createJavaExpression newValue))
	| TypedDeclaration((variableName, variableType), newValue) -> JavaStatement(JavaDeclaration(createJavaType variableType, variableName, Some(createJavaExpression newValue)))

and createJavaElseIf = function 
	TypedElseIfBlock(typedExpression, typedStatementBlock) -> JavaElseIf(createJavaExpression typedExpression, createJavaBlock typedStatementBlock)
and createJavaElse = function 
	TypedElseBlock(typedStatementBlock) -> JavaElse(createJavaBlock typedStatementBlock)
and createJavaBlock = function 
	TypedStatementBlock(typedStatementList) -> JavaBlock(List.map createJavaStatement typedStatementList)

let createJavaFunction = function
	  TypedFunctionDeclaration(functionHeader, TypedStatementBlock(typedStatementList)) -> 
	  		JavaFunction(functionHeader, JavaBlock(List.map createJavaStatement typedStatementList))

let createJavaMain = function
	  TypedStatementBlock(statementList) -> JavaMain(JavaBlock(List.map createJavaStatement statementList))

let createJavaMainClass typedFunctionBlockList = function 
	| TypedMainBlock(typedStatementList) -> JavaClass("Main", "", (createJavaMain typedStatementList)::(List.map createJavaFunction typedFunctionBlockList))

let listPairToMap gropuName keyList valueList = 
		JavaMap(
			gropuName,
			List.map string_of_int keyList, 
			List.map string_of_int valueList)

let listTablePairToMap gropuName keyList valueTable = 
	JavaMap(
		gropuName,
		List.concat (List.map (fun element1 -> List.map (fun element2 -> string_of_int element1 ^ "," ^ string_of_int element2) keyList) keyList),
		List.map string_of_int (List.concat valueTable))

let createJavaExtendedGroupClass = function
	| TypedGroupDeclaration(groupName, groupElements, additionTable, additiveInverseList) -> 
		 (JavaClass(
			groupName, 
			"Groups", 
			[JavaDefaultConstructor(
				groupName,
				JavaBlock(
					[JavaStatement(JavaConstant(listTablePairToMap "additionTable" groupElements additionTable));
					 JavaStatement(JavaConstant(listPairToMap "additiveInverseList" groupElements additiveInverseList))]))]))
	| TypedRingDeclaration(groupName, groupElements, additionTable, additiveInverseList, multiplicationTable) -> 
		 (JavaClass(
			groupName, 
			"Rings", 
			[JavaDefaultConstructor(
				groupName,
				JavaBlock(
					[JavaStatement(JavaConstant(listTablePairToMap "additionTable" groupElements additionTable));
					 JavaStatement(JavaConstant(listPairToMap "additiveInverseList" groupElements additiveInverseList));
					 JavaStatement(JavaConstant(listTablePairToMap "multiplicationTable" groupElements multiplicationTable))]))]))
	| TypedFieldDeclaration(groupName, groupElements, additionTable, additiveInverseList, multiplicationTable, multiplicitiveInverseList, additiveIdentity) -> 
		 (JavaClass(
			groupName, 
			"Fields", 
			[JavaDefaultConstructor(
				groupName,
				JavaBlock(
					[JavaStatement(JavaAssignment("additiveIdentity", JavaConstant(JavaValue(JavaInt(additiveIdentity)))));
					 JavaStatement(JavaConstant(listTablePairToMap "additionTable" groupElements additionTable));
					 JavaStatement(JavaConstant(listPairToMap "additiveInverseList" groupElements additiveInverseList));
					 JavaStatement(JavaConstant(listTablePairToMap "multiplicationTable" groupElements multiplicationTable));
					 JavaStatement(JavaConstant(listPairToMap "multiplicitiveInverseList" (List.filter (fun element -> element <> additiveIdentity) groupElements) multiplicitiveInverseList))]))]))

let createJavaAst = function
	  TypedProgram(typedMainBlock, typedFunctionBlockList, typedExtendedGroupBlockList) -> JavaClassList((createJavaMainClass typedFunctionBlockList typedMainBlock)::(List.map createJavaExtendedGroupClass typedExtendedGroupBlockList))
		
(* Generate code from Java Ast *)		
let generateJavaType logToJavaFile = function
	| JavaBooleanType -> logToJavaFile "boolean "
	| JavaIntType -> logToJavaFile "int "
	| JavaSetLiteralType -> logToJavaFile "PlatoSet<Object> "
	| JavaVectorLiteralType -> logToJavaFile "PlatoVector<Object> "
	| JavaNeutralType -> logToJavaFile "Object "

let generateJavaPrimitive logToJavaFile = function
	| JavaBoolean(booleanValue) -> logToJavaFile (string_of_bool booleanValue)
	| JavaInt(intValue) -> logToJavaFile (string_of_int intValue)

let rec generatePuts logToJavaFile mapName keyList valueList =
	(if List.length keyList > 0
	then (logToJavaFile (mapName ^ ".put(\"" ^ List.hd keyList ^ "\", \"" ^ List.hd valueList ^ "\");\n");
	     generatePuts logToJavaFile mapName (List.tl keyList) (List.tl valueList))
	else ())

let generateJavaValue logToJavaFile = function
	| JavaValue(javaPrimitive) -> generateJavaPrimitive logToJavaFile javaPrimitive
	| JavaMap(mapName, keyList, valueList) -> 
		logToJavaFile (mapName ^  " = new HashMap<String, String>();\n");
		generatePuts logToJavaFile mapName keyList valueList;
		logToJavaFile (mapName ^ " = Collections.unmodifiableMap(" ^ mapName ^ ")")

let rec generateJavaExpression logToJavaFile = function
	| JavaConstant(javaValue) -> generateJavaValue logToJavaFile javaValue
	| JavaVariable(stringValue) -> logToJavaFile stringValue
	| JavaReturn(expressionToReturn) ->
		logToJavaFile "return ";
		generateJavaExpression logToJavaFile expressionToReturn
	| JavaIf(javaExpression, javaBlock, javaElseIfList, javaElse) -> 
		logToJavaFile "if(";
		generateJavaExpression logToJavaFile javaExpression;
		logToJavaFile ")";
		generateJavaBlock logToJavaFile javaBlock;
		ignore (List.map (generateJavaElseIf logToJavaFile) javaElseIfList);
		generateJavaElse logToJavaFile javaElse
	| JavaIfNoElse(javaExpression, javaBlock, javaElseIfList) -> 
		logToJavaFile "if(";
		generateJavaExpression logToJavaFile javaExpression;
		logToJavaFile ")";
		generateJavaBlock logToJavaFile javaBlock;
		ignore (List.map (generateJavaElseIf logToJavaFile) javaElseIfList)
	| JavaAssignment(variableName, variableValue) -> 
		logToJavaFile (variableName ^ "=");
		generateJavaExpression logToJavaFile variableValue
	| JavaVectorAssignment(variableName, indexValue, variableValue) -> 
		logToJavaFile (variableName ^ ".set((");
		generateJavaExpression logToJavaFile indexValue;
		logToJavaFile (")-1, ");
		generateJavaExpression logToJavaFile variableValue;
		logToJavaFile (")");
	| JavaDeclaration(variableType, variableName, variableValue) ->
	  generateJavaType logToJavaFile variableType;
		logToJavaFile (variableName ^ "=");
		(match variableValue with
		  | Some(javaExpressionValue) -> generateJavaExpression logToJavaFile javaExpressionValue
		  | None -> () (* do nothing *))
	| JavaCall(className, methodName, javaExpressionList) ->
		if (methodName = "")
		then (generateJavaParameters logToJavaFile javaExpressionList)
		else if (className = "")
		     then (logToJavaFile (methodName ^ "(");
					    generateJavaParameters logToJavaFile javaExpressionList;
							logToJavaFile ")")
	       else let invokationString = (if String.contains className '.' then className else "(new " ^ className ^ "())") 
					    in logToJavaFile (invokationString ^ "." ^ methodName ^ "(");
				         generateJavaParameters logToJavaFile javaExpressionList;
				         logToJavaFile ")"
and generateJavaParameters logToJavaFile = function
	| [] -> ()
	| [first] -> ignore (generateJavaExpression logToJavaFile first)
	| first::rest -> ignore (generateJavaExpression logToJavaFile first);
	                 ignore (List.map (fun elem -> logToJavaFile ","; generateJavaExpression logToJavaFile elem) rest) 
and generateJavaElseIf logToJavaFile = function
	  JavaElseIf(javaExpression, javaBlock) ->
			logToJavaFile "else if("; 
			generateJavaExpression logToJavaFile javaExpression;
			logToJavaFile ")";
			generateJavaBlock logToJavaFile javaBlock
and generateJavaElse logToJavaFile = function
	  JavaElse(javaBlock) ->
			logToJavaFile "else"; 
			generateJavaBlock logToJavaFile javaBlock
and generateJavaBlock logToJavaFile = function
	  JavaBlock(javaStatementList) ->
			logToJavaFile "{\n"; 
			ignore (List.map (generateJavaStatement logToJavaFile) javaStatementList);
			logToJavaFile "}\n"
and generateJavaStatement logToJavaFile = function
	  JavaStatement(javaExpression) ->
			generateJavaExpression logToJavaFile javaExpression;
			(match javaExpression with 
				JavaIf(_, _, _, _) -> ()
				| JavaIfNoElse(_, _, _) -> ()
				| _ -> logToJavaFile ";\n")
			

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
				  | VoidType -> ignore (logToJavaFile "void ")
				  | OtherType(returnType) -> ignore (generateJavaType logToJavaFile (createJavaType returnType));
				);
	  		logToJavaFile functionHeader.functionName;
	  		logToJavaFile "(";
	  		(match functionHeader.parameters with
				  [] -> ()
				  | [first] -> ignore (generateJavaFunctionParameter logToJavaFile first)
				  | first::rest -> 
						ignore (generateJavaFunctionParameter logToJavaFile first);
				    ignore (List.map (fun elem -> logToJavaFile ","; generateJavaFunctionParameter logToJavaFile elem) rest));
	  		logToJavaFile ") ";
			generateJavaBlock logToJavaFile javaBlock)
		| JavaDefaultConstructor(className, javaBlock) ->
			(logToJavaFile ("public " ^ className ^ "()");
			 generateJavaBlock logToJavaFile javaBlock)

let removeFile fileName = 
	if Sys.file_exists fileName
	then Sys.remove fileName

let generateJavaClass fileName = function
	  JavaClass(javaClassName, javaSuperClassName, javaMethodList) -> 
			let fullClassName = (if javaClassName = "Main" then (javaClassName ^ "_" ^ fileName) else javaClassName)
			in let fullFileName = fullClassName ^ ".java"
				 in removeFile fullFileName;
			     let logToJavaFile = logToFileAppend false fullFileName
				   in let extendsString = (if javaSuperClassName = "" then "" else ("extends " ^ javaSuperClassName))
					    in logToJavaFile "import java.util.*;\n\n";
						     logToJavaFile (String.concat " " ["public class"; fullClassName; extendsString; "{\n"]);  
				         ignore (List.map (generateJavaMethod logToJavaFile) javaMethodList);
			           logToJavaFile "}\n"
		
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

let generatePlatoVectorLiteralsClass = 
	let logToIntegerClassFile = logToFileOverwrite false "VectorLiterals.java"
	in logToIntegerClassFile vectorLiteralsClassString

let generatePlatoVectorClass = 
	let logToIntegerClassFile = logToFileOverwrite false "PlatoVector.java"
	in logToIntegerClassFile platoVectorClassString

let generatePlatoGroupClass = 
	let logToGroupClassFile = logToFileOverwrite false "Groups.java"
	in logToGroupClassFile groupClassString		
	
let generatePlatoRingClass = 
	let logToRingClassFile = logToFileOverwrite false "Rings.java"
	in logToRingClassFile ringClassString	
	
let generatePlatoFieldClass = 
	let logToFieldClassFile = logToFileOverwrite false "Fields.java"
	in logToFieldClassFile fieldClassString	

let generatePlatoClasses = 
	generatePlatoBooleanClass;
	generatePlatoIntegerClass;
	generatePlatoSetLiteralsClass;
	generatePlatoSetClass;
	generatePlatoVectorLiteralsClass;
	generatePlatoVectorClass;
	generatePlatoGroupClass;	
	generatePlatoRingClass;
	generatePlatoFieldClass	
			
let generateJavaCode fileName = function
	  JavaClassList(javaClassList) -> 
			generatePlatoClasses;
			ignore (List.map (generateJavaClass fileName) javaClassList)

let compile fileName =
  let lexbuf = Lexing.from_channel (open_in fileName) 
	in let programAst = Parser.program Scanner.token lexbuf
	   in logProgramAst programAst; 
		    let programSast = checkProgram emptyGlobalEnvironment programAst
		    in logProgramSast programSast; 
				   let javaClassListAst = createJavaAst programSast
		       in logJavaClassListAst javaClassListAst;
					    let basename =  Filename.basename fileName
					    in if (String.sub basename ((String.length basename) - 4) 4) = ".plt"
							   then generateJavaCode (Filename.chop_extension basename) javaClassListAst
                 else raise(PlatoError("Invalid file extension"))
let _ = compile Sys.argv.(1)
	 
