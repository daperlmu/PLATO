{ open Parser open Logger}

rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf }
	| "BOOLEAN" { BOOLEAN_TYPE }
	| "INTEGER" { INTEGER_TYPE }
	| "NUMBER" { NUMBER_TYPE }
	| "VOID" { VOID_TYPE }
  | "TRUE" { BOOLEAN(true) }
	| "FALSE" { BOOLEAN(false) }
	| "NOT" { NOT }
	| "OR" { OR }
	| "AND" { AND }
	| "OVER" { OVER }
	| "PRINT" { PRINT }
	| "RETURN" { RETURN }
	| "GROUP" { GROUP }
	| "RING" { RING }
	| "FIELD" { FIELD }
	| "add" { ADD }
	| "multiply" { MULTIPLY }
	| "main()" { MAIN_HEADER }
	| '+' { PLUS }
	| '-' { MINUS }
	| '*' { TIMES }
	| '/' { DIVIDE }
	| '%' { PERCENT }
	| '^' { CARET }
	| '<' { LESS_THAN }
	| '>' { GREATER_THAN }
	| '=' { EQUAL }
	| ':' { COLON }
	| ',' { COMMA }
  | ';' { SEMICOLON }
	| '{' { OPEN_BRACE }
	| '}' { CLOSE_BRACE }
	| '[' { OPEN_BRACKET }
	| ']' { CLOSE_BRACKET }
	| '(' { LPAREN }
	| ')' { RPAREN }
	| '0' { NUMBER(0) }
  | ['1'-'9']['0'-'9']* as number { NUMBER(int_of_string number) }
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']* as identifier { IDENTIFIER(identifier) }
  | eof { EOF } 
