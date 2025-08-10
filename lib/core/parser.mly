/* This file defines a parser for a simple language, using OCaml's Menhir tool. */

%token <int> INT
%token <string> STR
%token LPAREN RPAREN SEMICOLON COMMA
%token PLUS MINUS TIMES DIVIDE MOD EOF
%token TRUE FALSE
%token NOT AND OR
%token EQ NEQ LEQ LT GT GTE
%token FORALL EXISTS IMPLIES
%token SKIP ASSUME ASSERT LET IN IF THEN ELSE WHILE DO 

%start <Ast.command> program
%type <Ast.arithmetic> arith
%type <Ast.boolean> bool
%type <Ast.command> command

/* Operator precedence (lowest to highest) */
%left SEMICOLON          /* Command sequencing */
%left OR
%left AND
%right IMPLIES
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc IN DO ELSE /* Control flow keywords */

%%

program:
    | command EOF { $1 }
;

arith:
    | i = INT { Ast.Int i }
    | s = STR { Ast.Var s }
    | LPAREN a = arith RPAREN { a }
    | MINUS a = arith { Ast.Neg a }
    | a1 = arith PLUS a2 = arith { Ast.Add (a1, a2) }
    | a1 = arith MINUS a2 = arith { Ast.Add (a1, Ast.Neg a2) }
    | a1 = arith TIMES a2 = arith { Ast.Mul (a1, a2) }
    | a1 = arith DIVIDE a2 = arith { Ast.Div (a1, a2) }
    | a1 = arith MOD a2 = arith { Ast.Mod (a1, a2) }
;

bool:
    | simple_bool { $1 }
    | b1 = bool OR b2 = bool { Ast.Or (b1, b2) }
    | b1 = bool AND b2 = bool { Ast.And (b1, b2) }
    | b1 = bool IMPLIES b2 = bool { Ast.Or (Ast.Not b1, b2) }
;

simple_bool:
    | TRUE { Ast.True }
    | FALSE { Ast.False }
    | a1 = arith EQ a2 = arith { Ast.Eq (a1, a2) }
    | a1 = arith NEQ a2 = arith { Ast.Not (Ast.Eq (a1, a2)) }
    | a1 = arith LT a2 = arith { Ast.And (Ast.Leq (a1, a2), Ast.Not (Ast.Eq (a1, a2))) }
    | a1 = arith LEQ a2 = arith { Ast.Leq (a1, a2) }
    | a1 = arith GT a2 = arith { Ast.Not (Ast.Leq (a1, a2)) }
    | a1 = arith GTE a2 = arith { Ast.Or (Ast.Not (Ast.Leq (a1, a2)), Ast.Eq (a1, a2)) }
    | NOT b = bool { Ast.Not b }
    | FORALL s = STR COMMA b = bool { Ast.Forall ([s], b) }
    | EXISTS s = STR COMMA b = bool { Ast.Not (Ast.Forall ([s], Ast.Not b)) }
    | LPAREN b = bool RPAREN { b }
;


command:
    | simple_command { $1 }
    | c1 = command SEMICOLON c2 = command { Ast.Seq (c1, c2) }
;

simple_command:
    | SKIP { Ast.Skip }
    | ASSUME m = bool { Ast.Assume m }
    | ASSERT m = bool { Ast.Assert m }
    | LET s = STR EQ a = arith IN c = command %prec IN { Ast.Let (s, a, c) }
    | IF b = bool THEN c1 = command ELSE c2 = command %prec ELSE { Ast.If (b, c1, c2) }
    | WHILE b = bool DO c = command %prec DO { Ast.While (b, c) }
    | LPAREN c = command RPAREN { c }
;