%{ open Ast open Logger %}

%token BOOLEAN_TYPE INTEGER_TYPE NUMBER_TYPE VOID_TYPE
%token NOT NEGATION
%token LESS_THAN GREATER_THAN EQUAL
%token PLUS MINUS BACKSLASH TIMES DIVIDE PERCENT CARET AND OR 
%token OVER PRINT RETURN GROUP RING FIELD ADD MULTIPLY
%token COLON COMMA SEMICOLON LPAREN RPAREN OPEN_BRACE CLOSE_BRACE MAIN_HEADER EOF OPEN_BRACKET CLOSE_BRACKET IF ELSEIF ELSE
%token <bool> BOOLEAN
%token <int> NUMBER
%token <string> IDENTIFIER

%left OR
%left AND
%left EQUAL
%left LESS_THAN LESS_THAN_OR_EQUAL GREATER_THAN GREATER_THAN_OR_EQUAL
%left PLUS MINUS BACKSLASH
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
  | platoType { OtherType($1) }

commaSeparatedExpressionNonemptyList:
	expression { [$1] }
	| commaSeparatedExpressionNonemptyList COMMA expression { $3::$1 }

setLiteral:
	OPEN_BRACE CLOSE_BRACE {SetLiteral([])}
	| OPEN_BRACE commaSeparatedExpressionNonemptyList CLOSE_BRACE {SetLiteral(List.rev $2)}

expression:
  | BOOLEAN { Boolean($1) }
	|	NUMBER { Number($1) }
	| IDENTIFIER { Identifier($1) }
	| NOT expression { Unop(Not, $2) }
	| MINUS expression %prec NEGATION	{ Unop(Negation, $2) }
  | expression OR expression { Binop(Or, $1, $3) }
	| expression AND expression { Binop(And, $1, $3) }
	| expression PLUS expression { Binop(Plus, $1, $3) }
	| expression MINUS expression { Binop(Minus, $1, $3) }
	| expression BACKSLASH expression { Binop(SetDifference, $1, $3) }
	| expression TIMES expression { Binop(Times, $1, $3) }
	| expression DIVIDE expression { Binop(Divide, $1, $3) }
	| expression PERCENT expression { Binop(Mod, $1, $3) }
	| expression CARET expression { Binop(Raise, $1, $3) }
	| expression LESS_THAN expression { Binop(LessThan, $1, $3) }
	| expression LESS_THAN EQUAL expression %prec LESS_THAN_OR_EQUAL { Binop(LessThanOrEqual, $1, $4) }
	| expression GREATER_THAN expression { Binop(GreaterThan, $1, $3) }
	| expression GREATER_THAN EQUAL expression %prec GREATER_THAN_OR_EQUAL { Binop(GreaterThanOrEqual, $1, $4) }
	| expression EQUAL expression { Binop(Equal, $1, $3) }
	| setLiteral {$1}
	| LPAREN expression RPAREN { $2 }

statement:
  | PRINT expression SEMICOLON { Print($2) }
  | RETURN expression SEMICOLON { Return($2) }
  | IF LPAREN expression RPAREN statementBlock elseIfBlockList elseBlock { If($3, $5, $6, $7) }
  | IF LPAREN expression RPAREN statementBlock elseIfBlockList { IfNoElse($3, $5, $6) }
  | IDENTIFIER COLON EQUAL expression SEMICOLON { Assignment($1, $4) }
	|	platoType IDENTIFIER COLON EQUAL expression SEMICOLON { Declaration($1, $2, $5) }

statementList:
  |/* empty */ { [] }
  | statementList statement { $2::$1 }

statementBlock: 
    OPEN_BRACE statementList CLOSE_BRACE { StatementBlock(List.rev $2) }

elseBlock:
	ELSE statementBlock { ElseBlock($2) }

elseIfBlock:
	ELSEIF LPAREN expression RPAREN statementBlock { ElseIfBlock($3, $5) }

elseIfBlockList:
	| { [] }
	| elseIfBlockList elseIfBlock { $2::$1 }

parameter:
	platoType IDENTIFIER { Parameter($1, $2) }

parameterWithComma:
  | parameter COMMA parameter { [$3; $1]}
	| parameterWithComma COMMA parameter { $3::$1 }

parameterList:
	| { [] }
	| parameter { [$1] }
	| parameterWithComma { $1 }

addFunctionHeader:  INTEGER_TYPE ADD LPAREN INTEGER_TYPE IDENTIFIER COMMA INTEGER_TYPE IDENTIFIER RPAREN { { returnType = OtherType(NumberType("Integers"));
														 functionName = "add";
														 parameters = [Parameter(NumberType("Integers"), $5); Parameter(NumberType("Integers"), $8)]  } }
														
addFunctionBlock: addFunctionHeader statementBlock { FunctionDeclaration($1, $2) }

multiplyFunctionHeader:  INTEGER_TYPE MULTIPLY LPAREN INTEGER_TYPE IDENTIFIER COMMA INTEGER_TYPE IDENTIFIER RPAREN { { returnType = OtherType(NumberType("Integers"));
														 functionName = "multiply";
														 parameters = [Parameter(NumberType("Integers"), $5); Parameter(NumberType("Integers"), $8)]  } }
														
multiplyFunctionBlock: multiplyFunctionHeader statementBlock { FunctionDeclaration($1, $2) }

functionHeader:
  | platoFunctionType IDENTIFIER LPAREN parameterList RPAREN { { returnType = $1;
														 functionName = $2;
														 parameters = List.rev $4 } }
	| IDENTIFIER LPAREN parameterList RPAREN { { returnType = VoidType;
												 functionName = $1;
												 parameters = List.rev $3 } }											 

functionBlock:
	  functionHeader statementBlock { FunctionDeclaration($1, $2) }

functionBlockList:
  | { [] }
  | functionBlockList functionBlock { $2::$1 }

mainBlock:
    MAIN_HEADER statementBlock { MainBlock($2) }

groupHeader:
    GROUP IDENTIFIER { GroupHeader($2) }
		
groupBody:
  /* TODO this needs a real set, make sure to update $4 when fixing this */
     setLiteral SEMICOLON addFunctionBlock { GroupBody($1, $3) }

groupBlock: 
    groupHeader OPEN_BRACE groupBody CLOSE_BRACE { GroupDeclaration($1, $3) } 

extendedGroupHeader:
  | RING IDENTIFIER	{ RingHeader($2) }	
	| FIELD IDENTIFIER	{ FieldHeader($2) }		

extendedGroupBody:
  /* TODO this needs a real set, make sure to update $4 when fixing this */
     groupBody multiplyFunctionBlock { ExtendedGroupBody($1, $2) }
		
extendedGroupBlock:
  | groupHeader OPEN_BRACE groupBody CLOSE_BRACE { GroupDeclaration($1, $3) } 
  | extendedGroupHeader OPEN_BRACE extendedGroupBody CLOSE_BRACE { ExtendedGroupDeclaration($1, $3) }		
		
groupBlockList:
  | { [] }
	| groupBlockList extendedGroupBlock { $2 :: $1 }

program:
    mainBlock functionBlockList groupBlockList { Program($1, List.rev $2, List.rev $3) }
