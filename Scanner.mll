{ open Parser open Logger}

rule token = parse
    [' ' '\t' '\r' '\n'] { token lexbuf }
    | "/*"     { comment lexbuf }
	| "BOOLEAN" { BOOLEAN_TYPE }
	| "INTEGER" { INTEGER_TYPE }
	| "NUMBER" { NUMBER_TYPE }
	| "SET"   { SET_TYPE }
	| "VOID" { VOID_TYPE }
	| "VECTOR"   { VECTOR_TYPE }
	| "TO"   { VECTOR_TO }
	| "BY"   { VECTOR_BY }
  | "WHICH" { WHICH_QUANTIFIER }
	| "SOME" { SOME_QUANTIFIER }
	| "ALL" { ALL_QUANTIFIER }
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
	| "if" { IF }
	| "elseif" { ELSEIF }
	| "else" { ELSE }
	| "main()" { MAIN_HEADER }
	| '+' { PLUS }
	| '-' { MINUS }
	| '\\' { BACKSLASH }
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
  | _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

 and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
