/* File parsing.mly
   This file defines a parser for a simple language, using OCaml's Menhir tool. */

%{
    open Core.Ast
%}

%token <int> INT
%token <string> STR
%token LPAREN RPAREN LBRACK RBRACK SEMICOLON DOT
%token REQUIRES ENSURES INVARIANT

%token PLUS MINUS TIMES DIVIDE MOD POW

%token TRUE FALSE
%token NOT AND OR IMPLIES
%token FORALL EXISTS
%token EQ NEQ LEQ LT GT GTE

%token SKIP ASSIGN IF THEN ELSE WHILE DO 
%token EOF

%start <program> program
%type <arithmetic> arith
%type <boolean> bool
%type <formula> form
%type <command> command

/* Operator precedence (lowest to highest) */
%left SEMICOLON          /* Command sequencing */
%left OR
%left AND
%right IMPLIES /* Right associative */
%left PLUS MINUS
%left TIMES DIVIDE MOD

%%

program:
    REQUIRES LBRACK pre = form RBRACK
    ENSURES LBRACK post = form RBRACK
    body = command
    EOF { { precondition = pre; postcondition = post; body = body } }
;

arith:
    | LPAREN a = arith RPAREN { a }
    | i = INT { Int i }
    | s = STR { Var s }
    | MINUS a = arith { Neg a }
    | a1 = arith PLUS a2 = arith { Add (a1, a2) }
    | a1 = arith MINUS a2 = arith { Add (a1, Neg a2) }
    | a1 = arith TIMES a2 = arith { Mul (a1, a2) }
    | a1 = arith DIVIDE a2 = arith { Div (a1, a2) }
    | a1 = arith MOD a2 = arith { Mod (a1, a2) }
    | a1 = arith POW a2 = arith { Pow (a1, a2) }
;

bool:
    | simple_bool { $1 }
    | b1 = bool OR b2 = bool { Or (b1, b2) }
    | b1 = bool AND b2 = bool { And (b1, b2) }
;

simple_bool:
    | TRUE { True }
    | FALSE { False }
    | LPAREN b = bool RPAREN { b }
    | NOT b = bool { Not b }
    | a1 = arith EQ a2 = arith { Eq (a1, a2) }
    | a1 = arith NEQ a2 = arith { Not (Eq (a1, a2)) }
    | a1 = arith LT a2 = arith { Lt (a1, a2) }
    | a1 = arith LEQ a2 = arith { Leq (a1, a2) }
    | a1 = arith GT a2 = arith { Gt (a1, a2) }
    | a1 = arith GTE a2 = arith { Geq (a1, a2) }
;

form:
    | simple_formula { $1 }
    | f1 = form OR f2 = form { OrF (f1, f2) }
    | f1 = form AND f2 = form { AndF (f1, f2) }
    | f1 = form IMPLIES f2 = form { ImplyF (f1, f2) }
;

simple_formula:
    | TRUE { TrueF }
    | FALSE { FalseF }
    | LPAREN b = form RPAREN { b }
    | NOT b = form { NotF b }
    | FORALL s = STR DOT b = form { ForallF (s, b) }
    | EXISTS s = STR DOT b = form { ExistsF (s, b) }
    | a1 = arith EQ a2 = arith { EqF (a1, a2) }
    | a1 = arith NEQ a2 = arith { NotF (EqF (a1, a2)) }
    | a1 = arith LT a2 = arith { LtF (a1, a2) }
    | a1 = arith LEQ a2 = arith { LeqF (a1, a2) }
    | a1 = arith GT a2 = arith { GtF (a1, a2) }
    | a1 = arith GTE a2 = arith { GeqF (a1, a2) }
;

command:
    | simple_command { $1 }
    | c1 = command SEMICOLON c2 = command { Seq (c1, c2) }
;

simple_command:
    | SKIP { Skip }
    | s = STR ASSIGN a = arith { Assign (s, a) }
    | IF b = bool 
        THEN LPAREN c1 = command RPAREN 
        ELSE LPAREN c2 = command RPAREN { If (b, c1, c2) }
    | WHILE b = bool 
        INVARIANT LBRACK inv = form RBRACK 
        DO LPAREN c = command RPAREN { While (b, inv, c) }
;