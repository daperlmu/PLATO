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
	PlatoError("Error while generating group, ring or field " ^ groupName ^ ".  Could not find identity element")
		
let inverseException groupName element=
	PlatoError("Error while generating group, ring or field " ^ groupName ^ ".  Could not find inverse of " ^ (string_of_int element))

let voidFunctionHasReturnException functionName = PlatoError("Function: " ^ functionName ^ " is a void function. Void functions cannot return an expression")

let missingReturnStmtException functionName functionReturnType = PlatoError("Function: " ^ functionName ^ " is a typed function of type " ^ functionReturnType ^ ". Missing return statement. Expecting return statement of type " ^ functionReturnType ^ ".")

let incompatibleTypesReturnStmt functionName functionReturnType lastStmtType = PlatoError("Return statement incompatible types for the Function: " ^ functionName ^ ". Required: " ^ functionReturnType ^ ". Found: " ^ lastStmtType ^ ".")

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
		 else raise(undefinedVariableException variableName))
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
	| FunctionDeclaration({ returnType = OtherType(NumberType("Integers")); functionName = _; parameters = [Parameter(NumberType("Integers"), identifierName1); Parameter(NumberType("Integers"), identifierName2)]}, StatementBlock([javaStatement])) -> 
		evaluateSimpleStatement identifierName1 identifierName2 input1 input2 javaStatement
	| _ -> raise(PlatoError("Functions in groups, rings or fields can only be add or multiply"))

(* Convert Ast to Sast *)
let canCast fromType toType = 
  if fromType = toType
	then true
	else 
		match toType with
		| NumberType(_) -> (fromType = NumberType("Integers"))
		| _ -> false

type symbolTable = {
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

let convertParamToVarDec = function
	Parameter(variableType, variableName) -> (variableName, variableType)

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

(* TODO need to make this work only for rings *)
let canApplyTimes = function
	| [NumberType(numberType1); NumberType(numberType2)] -> (numberType1 = numberType2)
	| [SetLiteralType(arg1); SetLiteralType(arg2)] -> arg1=arg2
	| _ -> false

(* TODO need to make this work only for fields *)
let canApplyDivide = function
	| [NumberType(numberType1); NumberType(numberType2)] -> (numberType1 = numberType2)
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
	| If (expression, statementBlock, elseIfBlockList, elseBlock) -> 
		TypedIf(checkExpression environment expression, 
		checkStatementBlock environment statementBlock, 
		List.map checkElseIfBlock environment elseIfBlockList, 
		checkElseBlock environment elseBlock)
	| IfNoElse (expression, statementBlock, elseIfBlockList) -> 
		TypedIfNoElse(checkExpression environment expression, checkStatementBlock environment statementBlock, List.map checkElseIfBlock environment elseIfBlockList)
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
and checkStatementBlock environment = function
	  StatementBlock(statementList) -> TypedStatementBlock(List.map (checkStatement environment) statementList)
and checkElseIfBlock environment = function 
	ElseIfBlock(expression, statementBlock) -> 
		TypedElseIfBlock(checkExpression environment expression, checkStatementBlock environment statementBlock)
and checkElseBlock environment = function 
	ElseBlock(statementBlock) -> 
		TypedElseBlock(checkStatementBlock environment statementBlock)

let extractExpressionFromStmt environment = function 
	| Print(expression) -> checkExpression environment expression
	| Return(expression) -> checkExpression environment expression
	| If(expression,_, _, _) -> checkExpression environment expression
	| IfNoElse(expression, _, _) -> checkExpression environment expression
  | Assignment(variableName, newValue) -> checkExpression environment newValue 
	| Declaration(variableType, variableName, newValue) -> checkExpression environment newValue

let checkMainBlock = function
	  MainBlock(mainBlock) -> TypedMainBlock(checkStatementBlock emptyEnviroment mainBlock)

let rec getReturnStmtsHelper = function 
	[] -> []
	| Return(expression)::[] -> [Return(expression)]
	| Return(expression)::tail -> Return(expression)::getReturnStmtsHelper tail
	| _::tail -> getReturnStmtsHelper tail

let getReturnStmts = function 
	StatementBlock(statementList) -> 
		getReturnStmtsHelper(statementList)

let checkVoidFunction = function 
	FunctionDeclaration(functionHeader, statementBlock) ->
		let returnStmtsInFunctionBlock = getReturnStmts statementBlock
			in let functionEnvironment = emptyEnviroment 
			   in if (List.length returnStmtsInFunctionBlock)=0
			  	  then (ignore (List.map (updateScope functionEnvironment.scope) (List.map convertParamToVarDec functionHeader.parameters));
			  			 TypedFunctionDeclaration(functionHeader, checkStatementBlock functionEnvironment statementBlock))
			  	  else raise(voidFunctionHasReturnException functionHeader.functionName)

let getLastStmtInBlock = function
	StatementBlock(statementList) -> List.hd (List.rev statementList)

let checkFunctionBlock = function
	  FunctionDeclaration(functionHeader, statementBlock) -> 
	  	let functionReturnType = functionHeader.returnType
		  			in (match functionReturnType with
		  					VoidType -> checkVoidFunction (FunctionDeclaration(functionHeader, statementBlock))
		  					| OtherType(returnType) -> 
		  						let functionEnvironment = emptyEnviroment
		  							in let returnStmtsInFunctionBlock = getReturnStmts statementBlock
		  								in if (List.length returnStmtsInFunctionBlock)=0
	  									then raise(missingReturnStmtException functionHeader.functionName (Logger.typeToString returnType))
	  									else
  											(ignore (List.map (updateScope functionEnvironment.scope) (List.map convertParamToVarDec functionHeader.parameters));
		  										let checkedStatementBlock = checkStatementBlock functionEnvironment statementBlock
		  										(* 
													ALGORITHM:
													If there is an else block:
														if it has a return statement then:
															If the return statement in the else block is not the last statement in the else block, then raise(unreachable statement exception)
															-A return statement at the end of the function is not necessary iff:
																- For all other if and elseif blocks, there is a return statement at the end of every block respectively.
																	- If for any of the if or elseif blocks, the return statement is not the last statement, then raise(unreachable statement exception)

															If you reach this point, no exceptions have been raised, meaning the function safely returns a value no matter what.

														else if it does not have a return statement then:
															- Run through the current function validation code.

		  										 *)
				  								in let lastStmt = getLastStmtInBlock statementBlock
				  								   in let lastStmtType = getExpressionType (extractExpressionFromStmt functionEnvironment lastStmt)
				  									  in if not (lastStmtType=returnType)
				  										 then raise(incompatibleTypesReturnStmt functionHeader.functionName (Logger.typeToString returnType) (Logger.typeToString lastStmtType))
				  										 else TypedFunctionDeclaration(functionHeader, checkedStatementBlock))
				  												(*  - check if the returned expression can be cast up to the function's expected return type
				  													- need to check all return statements in the function to make sure they are type compatible with the expected return type
				  													- Check if last statement is actually a return statement
				  												- check for unreachable code *)

		  					)

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

let isClosed groupElements groupTable = 
	let allTrue list = List.fold_left (&&) true list
  in allTrue (List.map (fun intList -> allTrue (List.map (isElement groupElements) intList)) groupTable)	
																																																																																														
let checkAssociative a b c groupTable = 
	let groupTimes = fun a b -> List.nth (List.nth groupTable a) b
	in let starResult = groupTimes (groupTimes a b) c
	   in let circleResult = groupTimes a (groupTimes b c)
		    in starResult = circleResult
						
let rec checkAssociativeList aList b c groupTable	=
	match aList with
	| [] -> true
	| head::tail -> if checkAssociative head b c groupTable 
	                then checkAssociativeList tail b c groupTable	
		              else false
								
let rec checkAssociativeListPair aList bList c groupTable =	
	match bList with
	| [] -> true
	| head::tail -> if checkAssociativeList aList head c groupTable 
	                then checkAssociativeListPair aList tail c groupTable	
		              else false		
		
let rec checkAssociativeListTriple aList bList cList groupTable =	
	match cList with
	| [] -> true
	| head::tail -> if checkAssociativeListPair aList bList head groupTable 
	                then checkAssociativeListTriple aList bList tail groupTable	
		              else false				
					
let isAssociative groupElements groupTable = checkAssociativeListTriple groupElements groupElements groupElements groupTable 				

let rec getIndexHelper element list startIndex =
	match list with 
	| [] -> raise Not_found
	| head::tail -> 
		if element = head 
	  then startIndex
	  else getIndexHelper element tail (startIndex + 1)

let getIndex element list = getIndexHelper element list 0												
						
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

let checkDistributive a b c additionTable multiplicationTable = 
  let groupPlus = fun a b -> List.nth (List.nth additionTable a) b
	in let groupTimes = fun a b -> List.nth (List.nth multiplicationTable a) b
	   in let starResult = groupTimes a (groupPlus b c)
	      in let circleResult = groupPlus (groupTimes a b) (groupTimes a c)
		       in starResult = circleResult
						
let rec checkDistributiveList aList b c additionTable multiplicationTable	=
	match aList with
	| [] -> true
	| head::tail -> if checkDistributive head b c additionTable multiplicationTable 
	                then checkDistributiveList tail b c additionTable multiplicationTable	
		              else false
								
let rec checkDistributiveListPair aList bList c additionTable multiplicationTable =	
	match bList with
	| [] -> true
	| head::tail -> if checkDistributiveList aList head c additionTable multiplicationTable 
	                then checkDistributiveListPair aList tail c additionTable multiplicationTable	
		              else false		
		
let rec checkDistributiveListTriple aList bList cList additionTable multiplicationTable =	
	match cList with
	| [] -> true
	| head::tail -> if checkDistributiveListPair aList bList head additionTable multiplicationTable 
	                then checkDistributiveListTriple aList bList tail additionTable multiplicationTable	
		              else false				
					
let distributes groupElements additionTable multiplicationTable = checkDistributiveListTriple groupElements groupElements groupElements additionTable multiplicationTable
						
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
			   
let checkProgram = function
	  Program(mainBlock, functionBlockList, extendedGroupBlockList) -> TypedProgram(checkMainBlock mainBlock, List.map checkFunctionBlock functionBlockList, List.map checkExtendedGroupBlock extendedGroupBlockList)	 

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

let generateJavaElseIf logToJavaFile = function
	  JavaElseIf(javaExpression, javaBlock) ->
			logToJavaFile "elseif("; 
			generateJavaExpression logToJavaFile javaExpression;
			logToJavaFile ")";
			generateJavaBlock logToJavaFile javaBlock


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
		List.map (generateJavaElseIf logToJavaFile) javaElseIfList;
		generateJavaElse logToJavaFile javaElse
	| JavaIfNoElse(javaExpression, javaBlock, javaElseIfList) -> 
		logToJavaFile "if(";
		generateJavaExpression logToJavaFile javaExpression;
		logToJavaFile ")";
		generateJavaBlock logToJavaFile javaBlock;
		List.map (generateJavaElseIf logToJavaFile) javaElseIfList
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
			let invokationString = (if String.contains className '.' then className else "(new " ^ className ^ "())")
			in logToJavaFile (invokationString ^ "." ^ methodName ^ "(");
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
	generatePlatoCommonClass;
	generatePlatoBooleanClass;
	generatePlatoIntegerClass;
	generatePlatoSetLiteralsClass;
	generatePlatoSetClass;
	generatePlatoGroupClass;	
	generatePlatoRingClass;
	generatePlatoFieldClass	
			
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
	 
