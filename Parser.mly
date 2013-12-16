%{ open Ast open Logger %}

%token BOOLEAN_TYPE INTEGER_TYPE NUMBER_TYPE VOID_TYPE
%token NOT NEGATION
%token LESS_THAN GREATER_THAN EQUAL
%token PLUS MINUS TIMES DIVIDE PERCENT CARET AND OR 
%token OVER PRINT RETURN
%token COLON COMMA SEMICOLON LPAREN RPAREN OPEN_BRACE CLOSE_BRACE MAIN_HEADER EOF OPEN_BRACKET CLOSE_BRACKET
%token <bool> BOOLEAN
%token <int> NUMBER
%token <string> IDENTIFIER

%left OR
%left AND
%left EQUAL
%left LESS_THAN LESS_THAN_OR_EQUAL GREATER_THAN GREATER_THAN_OR_EQUAL
%left PLUS MINUS
%left TIMES DIVIDE PERCENT
%nonassoc NOT NEGATION
%right CARET
%left LPAREN RPAREN

%start program
%type <Ast.program> program

%%

platoType:
  | BOOLEAN_TYPE { BooleanType }
	| INTEGER_TYPE  { NumberType("Integers") }
	| NUMBER_TYPE  { NumberType("Integers") }
	| NUMBER_TYPE OVER IDENTIFIER { NumberType($3) }

platoFunctionType:
	| VOID_TYPE { VoidType }
	| BOOLEAN_TYPE { OtherType(BooleanType) }
	| INTEGER_TYPE  { OtherType(NumberType("Integers")) }
	| NUMBER_TYPE  { OtherType(NumberType("Integers")) }
	| NUMBER_TYPE OVER IDENTIFIER { OtherType(NumberType($3)) }

/*
restOfCommaSeparatedExpressionList
	*//* nothing *//* { [] }
	| COMMA expression expressionCommaSeparatedList {$2::$3}

commaSeparatedExpressionList
	*//* nothing *//* { [] }
	| expression restOfCommaSeparatedExpressionList {$1::$2}

setLiteral:
	OPEN_BRACE commaSeparatedExpressionList CLOSE_BRACE {$2}
*/

expression:
    BOOLEAN { Boolean($1) }
	|	NUMBER { Number($1) }
	| IDENTIFIER { Identifier($1) }
	| NOT expression { Unop(Not, $2) }
	| MINUS expression %prec NEGATION	{ Unop(Negation, $2) }
  | expression OR expression { Binop(Or, $1, $3) }
	| expression AND expression { Binop(And, $1, $3) }
	| expression PLUS expression { Binop(Plus, $1, $3) }
	| expression MINUS expression { Binop(Minus, $1, $3) }
	| expression TIMES expression { Binop(Times, $1, $3) }
	| expression DIVIDE expression { Binop(Divide, $1, $3) }
	| expression PERCENT expression { Binop(Mod, $1, $3) }
	| expression CARET expression { Binop(Raise, $1, $3) }
	| expression LESS_THAN expression { Binop(LessThan, $1, $3) }
	| expression LESS_THAN EQUAL expression %prec LESS_THAN_OR_EQUAL { Binop(LessThanOrEqual, $1, $4) }
	| expression GREATER_THAN expression { Binop(GreaterThan, $1, $3) }
	| expression GREATER_THAN EQUAL expression %prec GREATER_THAN_OR_EQUAL { Binop(GreaterThanOrEqual, $1, $4) }
	| expression EQUAL expression { Binop(Equal, $1, $3) }
	| LPAREN expression RPAREN { $2 }
	| OPEN_BRACE expression CLOSE_BRACE {SetLiteral($2)}
	
statement:
    PRINT expression SEMICOLON { Print($2) }
    /*| RETURN expression SEMICOLON { Return($2) }*/
  | IDENTIFIER COLON EQUAL expression SEMICOLON { Assignment($1, $4) }
	|	platoType IDENTIFIER COLON EQUAL expression SEMICOLON { Declaration($1, $2, $5) }

statementList:
    /* empty */ { [] }
  | statementList statement { $2::$1 }
	
statementBlock: 
    OPEN_BRACE statementList CLOSE_BRACE { StatementBlock(List.rev $2) }

parameter:
	platoType IDENTIFIER { Parameter($1, $2) }

parameterWithComma:
    parameter COMMA parameter { [$3; $1]}
	| parameterWithComma COMMA parameter { $3::$1 }

parameterList:
	{ [] }
	| parameter { [$1] }
	| parameterWithComma { $1 }

functionHeader:
	platoFunctionType IDENTIFIER LPAREN parameterList RPAREN { { returnType = $1;
														 functionName = $2;
														 parameters = List.rev $4 } }
	| IDENTIFIER LPAREN parameterList RPAREN { { returnType = VoidType;
												 functionName = $1;
												 parameters = List.rev $3 } }											 

functionBlock:
	functionHeader statementBlock { FunctionDeclaration($1, $2) }

mainBlock:
    MAIN_HEADER statementBlock { MainBlock($2) }

bodyBlockList:
  { [] }
  | bodyBlockList functionBlock { $2::$1 }

/*
TODO: Replace current definition of program with:
program:
    mainBlock bodyBlockList { Program($1, List.rev $2) }
    */
program:
    mainBlock bodyBlockList { Program($1, List.rev $2) }
