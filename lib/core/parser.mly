/* This file defines a parser for a simple language, using OCaml's Menhir tool. */

%token <int> INT
%token <string> STR
%token LPAREN RPAREN LBRACK RBRACK SEMICOLON COMMA
%token PLUS MINUS TIMES DIVIDE MOD EOF
%token TRUE FALSE
%token NOT AND OR IMPLIES
%token EQ NEQ LEQ LT GT GTE
%token SKIP ASSIGN IF THEN ELSE WHILE DO 

%start <Ast.command> program
%type <Ast.arithmetic> arith
%type <Ast.boolean> bool
%type <Ast.formula> form
%type <Ast.command> command

/* Operator precedence (lowest to highest) */
%left SEMICOLON          /* Command sequencing */
%left OR
%left AND
%right IMPLIES
%left PLUS MINUS
%left TIMES DIVIDE MOD
%nonassoc DO ELSE /* Control flow keywords */

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
    | LPAREN b = bool RPAREN { b }
;

form:
    | simple_formula { $1 }
    | f1 = form OR f2 = form { Ast.OrF (f1, f2) }
    | f1 = form AND f2 = form { Ast.AndF (f1, f2) }
    | f1 = form IMPLIES f2 = form { Ast.ImplyF (f1, f2) }
;

simple_formula:
    | TRUE { Ast.TrueF }
    | FALSE { Ast.FalseF }
    | a1 = arith EQ a2 = arith { Ast.EqF (a1, a2) }
    | a1 = arith NEQ a2 = arith { Ast.NotF (Ast.EqF (a1, a2)) }
    | a1 = arith LT a2 = arith { Ast.AndF (Ast.LeqF (a1, a2), Ast.NotF (Ast.EqF (a1, a2))) }
    | a1 = arith LEQ a2 = arith { Ast.LeqF (a1, a2) }
    | a1 = arith GT a2 = arith { Ast.NotF (Ast.LeqF (a1, a2)) }
    | a1 = arith GTE a2 = arith { Ast.OrF (Ast.NotF (Ast.LeqF (a1, a2)), Ast.EqF (a1, a2)) }
    | NOT b = form { Ast.NotF b }
    | LPAREN b = form RPAREN { b }
;

command:
    | simple_command { $1 }
    | c1 = command SEMICOLON c2 = command { Ast.Seq (c1, c2) }
;

simple_command:
    | SKIP { Ast.Skip }
    | s = STR ASSIGN a = arith { Ast.Assign (s, a) }
    | IF b = bool THEN c1 = command ELSE c2 = command %prec ELSE { Ast.If (b, c1, c2) }
    | WHILE b = bool LBRACK inv = form RBRACK DO c = command %prec DO { Ast.While (b, inv, c) }
    | LPAREN c = command RPAREN { c }
;