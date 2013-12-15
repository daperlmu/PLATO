open Ast;;
open Logger;;
open Sast;;
open JavaAst;;
open Filename;;

exception PlatoError of string

let undefinedException variableName =
	PlatoError("Undeclared identifier " ^ variableName)

let castException expressionType  variableType = 
	PlatoError("Cannot cast from " ^ (typeToString expressionType) ^ " to " ^ (typeToString variableType))

(* Convert Ast to Sast *)
let canCast fromType toType = 
  if fromType = toType
	then true
	else false (* TODO we need to add autocasting here *)


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
	  TypedBoolean(_, expressionType) -> expressionType
	| TypedNumber(_, expressionType) -> expressionType
  | TypedIdentifier(_, expressionType) -> expressionType
	| TypedUnop(_, expressionType, _) -> expressionType
	| TypedBinop(_, expressionType, _, _) -> expressionType

let getOperatorTypes = function
	(* TODO should be numbers no integers *)
	  Not -> [BooleanType]
	| And -> [BooleanType; BooleanType]
	| Or -> [BooleanType; BooleanType]
	| Negation -> [IntegerType]
  | Plus -> [IntegerType; IntegerType]
	| Minus -> [IntegerType; IntegerType]
	| Times -> [IntegerType; IntegerType]
	| Divide -> [IntegerType; IntegerType]
	| Mod -> [IntegerType; IntegerType]
	| Raise -> [IntegerType; IntegerType]
	| LessThan -> [IntegerType; IntegerType]
	| LessThanOrEqual -> [IntegerType; IntegerType]
	| GreaterThan -> [IntegerType; IntegerType]
	| GreaterThanOrEqual -> [IntegerType; IntegerType]
	| Equal -> [IntegerType; IntegerType]

let getOperatorReturnType = function
 (* TODO should be numbers no integers *) 
	  Not -> BooleanType
	| And -> BooleanType
	| Or -> BooleanType
	| Negation -> IntegerType
  | Plus -> IntegerType
	| Minus -> IntegerType
	| Times -> IntegerType
	| Divide -> IntegerType
	| Mod -> IntegerType
	| Raise -> IntegerType
	| LessThan -> BooleanType
	| LessThanOrEqual -> BooleanType
	| GreaterThan -> BooleanType
	| GreaterThanOrEqual -> BooleanType
	| Equal -> BooleanType

let rec checkExpression environment = function
	  Boolean(booleanValue) -> TypedBoolean(booleanValue, BooleanType)
	| Number(numberValue) -> TypedNumber(numberValue, IntegerType)
  | Identifier(variableName) -> 
		  let variableDeclaration = 
				try findVariable environment.scope variableName 
			  with Not_found -> raise (undefinedException variableName)
		  in  let (_, variableType) = variableDeclaration
			    in TypedIdentifier(variableName, variableType)
	| Unop(unaryOperator, unopExpression) ->
		(let unaryExpression =  checkExpression environment unopExpression
		in match getOperatorTypes unaryOperator with
		       [operatorType] -> 
						let expressionType = (getExpressionType unaryExpression)
			      in if canCast expressionType operatorType
			         then TypedUnop(unaryOperator, getOperatorReturnType unaryOperator, unaryExpression)
					     else raise(castException expressionType operatorType)
			   | _ -> raise(PlatoError("Unop must have exactly have one input type")))
	| Binop(binaryOperator, binaryExpression1, binaryExpression2) ->
		(let binaryExpression1 = checkExpression environment binaryExpression1
		and binaryExpression2 = checkExpression environment binaryExpression2
		in match getOperatorTypes binaryOperator with
		       [operatorType1; operatorType2] -> 
						let expressionType1, expressionType2 = (getExpressionType binaryExpression1), (getExpressionType binaryExpression2)
			      in if canCast expressionType1 operatorType1
			         then if canCast expressionType2 operatorType2
					          then TypedBinop(binaryOperator, getOperatorReturnType binaryOperator, binaryExpression1, binaryExpression2)
							      else raise(castException expressionType2 operatorType1)
					     else raise(castException expressionType1 operatorType2)
		     | _ -> raise(PlatoError("Binop must have exactly have two input types")))

let rec checkStatement environment = function
	  Print(expression) -> TypedPrint(checkExpression environment expression)
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
let createJavaOperator = function
	  Not -> JavaNot
	| And -> JavaAnd
	| Or -> JavaOr
	| Negation -> JavaNegation
  | Plus -> JavaPlus
	| Minus -> JavaMinus
	| Times -> JavaTimes
	| Divide -> JavaDivide
	| Mod -> JavaMod
	| Raise -> JavaOperator(JavaCall("Math.pow", []))
	| LessThan -> JavaLessThan
	| LessThanOrEqual -> JavaLessThanOrEqual
	| GreaterThan -> JavaGreaterThan
	| GreaterThanOrEqual -> JavaGreaterThanOrEqual
	| Equal -> JavaEqual

let createJavaType = function
	  BooleanType -> Bool
  | IntegerType -> Int
	| NumberType(_) -> Int

let rec createJavaExpression = function
	(* TODO need to generate casts here *)
	  TypedBoolean(booleanValue, _) -> JavaBoolean(booleanValue)
	| TypedNumber(numberValue, _)-> JavaInt(numberValue)
  | TypedIdentifier(variableName, _) -> JavaVariable(variableName)
	| TypedUnop(unaryOperator, operatorType, unopExpression) ->
		JavaUnop(createJavaOperator unaryOperator, createJavaExpression unopExpression)
	| TypedBinop(binaryOperator, operatorType, binaryExpression1, binaryExpression2) ->
		JavaBinop(createJavaOperator binaryOperator, createJavaExpression binaryExpression1, createJavaExpression binaryExpression2)

let createJavaStatement = function
	  TypedPrint(expression) -> JavaStatement(JavaExpression(JavaCall("System.out.println", [createJavaExpression expression])))
	| TypedAssignment((variableName, variableType), newValue) -> JavaStatement(JavaAssignment(variableName, createJavaExpression newValue))
	| TypedDeclaration((variableName, variableType), newValue) -> JavaStatement(JavaDeclaration(createJavaType variableType, variableName, Some(createJavaExpression newValue)))

let createJavaMain = function
	  TypedStatementBlock(statementList) -> JavaMain(JavaBlock(List.map createJavaStatement statementList))

let createJavaClass = function 
	  TypedMainBlock(typedStatementList) -> [JavaClass("main", [createJavaMain typedStatementList])]

let createJavaAst = function
	  TypedProgram(typedMainBlock) -> JavaClassList(createJavaClass typedMainBlock)
		
(* Generate code from Java Ast *)		
let generateJavaOperator logToFile = function 
	  JavaNot -> logToFile "!"
	| JavaAnd -> logToFile "&&"
	| JavaOr -> logToFile "||"
	| JavaNegation -> logToFile "-"
  | JavaPlus -> logToFile "+"
	| JavaMinus -> logToFile "-"
	| JavaTimes -> logToFile "*"
	| JavaDivide -> logToFile "/"
	| JavaMod -> logToFile "%"
	| JavaOperator(JavaCall(methodName, _)) -> raise(Invalid_argument(methodName))
	| JavaLessThan -> logToFile "<"
	| JavaLessThanOrEqual -> logToFile "<="
	| JavaGreaterThan -> logToFile ">"
	| JavaGreaterThanOrEqual -> logToFile ">="
	| JavaEqual -> logToFile "=="

let generateJavaType logToJavaFile = function
	  Bool -> logToJavaFile "boolean "
	| Int -> logToJavaFile "int "

let rec generateJavaCall logToJavaFile = function
	  JavaCall(methodName, javaExpressionList) ->
			logToJavaFile (String.concat "" [methodName; "("]);
			(* TODO need commas if there are multiple arguments *)
			ignore (List.map (generateJavaExpression logToJavaFile) javaExpressionList);
			logToJavaFile ")"
and generateJavaExpression logToJavaFile = function
	  JavaBoolean(booleanValue) -> logToJavaFile (string_of_bool booleanValue)
	| JavaInt(intValue) -> logToJavaFile (string_of_int intValue)
	| JavaVariable(stringValue) -> logToJavaFile stringValue
	| JavaUnop(javaOperator, javaExpression) -> 
		generateJavaOperator logToJavaFile javaOperator;
		logToJavaFile "(";
		generateJavaExpression logToJavaFile javaExpression;
		logToJavaFile ")"
	| JavaBinop(javaOperator, javaExpression1, javaExpression2) -> 
		(match javaOperator with
		    JavaOperator(JavaCall(methodName, [])) -> generateJavaCall logToJavaFile (JavaCall(methodName, [javaExpression1; javaExpression2]))
		  | _ -> logToJavaFile "(";
				     generateJavaExpression logToJavaFile javaExpression1;
						 logToJavaFile ")";
				     generateJavaOperator logToJavaFile javaOperator;
						 logToJavaFile "(";
						 generateJavaExpression logToJavaFile javaExpression2;
						 logToJavaFile ")")
  | JavaExpression(javaCall) -> generateJavaCall logToJavaFile javaCall
	| JavaAssignment(variableName, variableValue) -> 
		logToJavaFile (variableName ^ "=");
		generateJavaExpression logToJavaFile variableValue
	| JavaDeclaration(variableType, variableName, variableValue) ->
	  generateJavaType logToJavaFile variableType;
		logToJavaFile (variableName ^ "=");
		match variableValue with
		  | Some(javaExpressionValue) -> generateJavaExpression logToJavaFile javaExpressionValue
		  | None -> () (* do nothing *)

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
			
let generateJavaCode fileName = function
	  JavaClassList(javaClassList) -> ignore (List.map (generateJavaClass fileName) javaClassList)

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
	 
