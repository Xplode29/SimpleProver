(* File lexer.mll
   This file defines a lexer for a simple language, recognizing integers and strings.
   It uses OCaml's ocamllex tool to generate the lexer from this specification.
*)

{
    open Parser        (* The type token is defined in parser.mli *)
    exception Eof
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let string = letter+
let newline = '\n'

rule read = parse
    | white { read lexbuf } (* Skip whitespace *)
    | newline { read lexbuf }

    | '(' { LPAREN } | ')' { RPAREN } | ';' { SEMICOLON } | ',' { COMMA }
    | '+' { PLUS } | '-' { MINUS } | '*' { TIMES } | '/' { DIVIDE } | '%' { MOD } | eof { EOF }
    | "true" { TRUE } | "false" { FALSE }
    | '!' | "not" { NOT } | "&&" | "and" { AND } | "||" | "or" { OR }
    | '=' { EQ } | "!=" { NEQ } | "<=" { LEQ } | '<' { LT } | '>' { GT } | ">=" { GTE }
    | "forall" { FORALL } | "exists" { EXISTS } | "implies" | "=>" { IMPLIES }
    | "skip" { SKIP } | "assume" { ASSUME } | "assert" { ASSERT } | "let" { LET } | "in" { IN } | "if" { IF } | "then" { THEN } | "else" { ELSE } | "while" { WHILE } | "do" { DO }

    | int as i { INT (int_of_string i) }
    | string as s { STR s }

    | eof { raise Eof } (* End of file *)
    | _ as c { raise (Failure ("Unexpected character: " ^ String.make 1 c)) } (* Some error handling *)