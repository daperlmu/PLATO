open Ast;;
open Logger;;
open Sast;;
open JavaAst;;
open Filename;;

exception PlatoError of string

let undefinedException variableName =
	PlatoError("Undeclared identifier " ^ variableName)

let typeToString = function
	  BooleanType -> "boolean"
  | IntegerType -> "integer"
	| NumberType(groupName) -> "number"
let castException expressionType  variableType = 
	PlatoError("Cannot cast from " ^ (typeToString expressionType) ^ " to " ^ (typeToString variableType))

(* Convert Ast to Sast *)
let canCast fromType toType = 
  if fromType = toType
	then true
	else false (* TODO we need to add autocasting here *)


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

let rec checkExpression environment = function
	  Boolean(booleanValue) -> TypedBoolean(booleanValue), BooleanType
	| Number(numberValue) -> TypedNumber(numberValue), IntegerType
  | Identifier(variableName) -> 
		  let variableDeclaration = 
				try findVariable environment.scope variableName 
			  with Not_found -> raise (undefinedException variableName)
		  in  let (_, variableType) = variableDeclaration
			    in TypedIdentifier(variableName), variableType

let rec checkStatement environment = function
	  Print(expression) -> TypedPrint(checkExpression environment expression)
  | Assignment(variableName, newValue) -> 
		let variableIdentifier = Identifier(variableName) 
		in let (_, variableType) as variableDetails = checkExpression environment variableIdentifier
			 in let (_, expressionType) as expressionDetails = checkExpression environment newValue
			     in if canCast expressionType variableType
				      then TypedAssignment((variableName, variableType), expressionDetails) 
						  else raise(castException expressionType variableType)
	| Declaration(variableType, variableName, newValue) ->
		let (checkedValue, expressionType) as expressionDetails = checkExpression environment newValue
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
	  BooleanType -> Bool
  | IntegerType -> Int
	| NumberType(_) -> Int

let createJavaExpression = function
	  TypedBoolean(booleanValue), _ -> JavaBoolean(booleanValue)
	| TypedNumber(numberValue), _ -> JavaInt(numberValue)
  | TypedIdentifier(variableName), _ -> JavaVariable(variableName)

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
let generateJavaType logToJavaFile = function
	  Bool -> logToJavaFile "boolean "
	| Int -> logToJavaFile "int "

let rec generateJavaCall logToJavaFile = function
	  JavaCall(methodName, javaExpressionList) ->
			logToJavaFile (String.concat "" [methodName; "("]);
			ignore (List.map (generateJavaExpression logToJavaFile) javaExpressionList);
			logToJavaFile ")"
and generateJavaExpression logToJavaFile = function
	  JavaBoolean(booleanValue) -> logToJavaFile (string_of_bool booleanValue)
	| JavaInt(intValue) -> logToJavaFile (string_of_int intValue)
	| JavaVariable(stringValue) -> logToJavaFile stringValue
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
			in let logToJavaFile = logToFileNoNewline (String.concat "" [fullClassName; ".java"])
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
	 
