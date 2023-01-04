%{
open Ast

let rec unfoldProcList (dpList : declProc list) : declProc =
  match dpList with
  | [] -> NullProc
  | [dp] -> dp
  | dp1 :: dp2 :: t -> unfoldProcList ((DSeqProc (dp1, dp2)) :: t)
%}

%token TRUE
%token FALSE
%token NOT
%token AND
%token OR
%token PLUS
%token MINUS
%token MUL
%token EQ
%token LEQ
%token <string> ID
%token <string> CONST

%token SKIP
%token TAKES
%token SEQ
%token IF
%token THEN
%token ELSE
%token REPEAT
%token FOREVER
%token PROC
%token VAL
%token ARRAY
%token BREAK
%token REF

%token LPAREN 
%token LBRACE 
%token RBRACE 
%token LBRACK
%token RBRACK
%token RPAREN
%token EOF
%token INT


%left SEQ
%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left MUL
%nonassoc RPAREN



%start <prog> prog

%%

prog:
  | dV=declVar;  dP = declProc; c = cmd; EOF { Prog(dV, dP, c) }
;

expr:
  | n = CONST { Const(int_of_string n) }
  | TRUE { True }
  | FALSE { False }
  | NOT; e=expr { Not(e) }
  | e1=expr; AND; e2=expr { And(e1,e2) }
  | e1=expr; OR; e2=expr { Or(e1,e2) }
  | e1=expr; PLUS; e2=expr { Add(e1,e2) }
  | e1=expr; MINUS; e2=expr { Sub(e1,e2) }
  | e1=expr; MUL; e2=expr { Mul(e1,e2) }
  | e1=expr; EQ; e2=expr { Eq(e1,e2) }
  | e1=expr; LEQ; e2=expr { Leq(e1,e2) }
  | x = ID { Var(x) }
  | LPAREN; e = expr; RPAREN { e }
  | ide = ID; LBRACK ; e = expr; RBRACK { Arr(ide, e) }
;

cmd:
  | SKIP { Skip }
  | BREAK { Break }
  | LPAREN; c = cmd; RPAREN { c }
  | LBRACE; dv = declVar ; c = cmd; RBRACE { Block(dv, c) } 
  | IF; e0 = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e0,c1,c2) }
  | x = ID; TAKES; e = expr; { Assign(x,e) }
  | c1 = cmd; SEQ; c2 = cmd; { Seq(c1,c2) }
  | c1 = cmd; SEQ { c1 }
  | ide = ID; LBRACK ; i = CONST; RBRACK; TAKES ; e = expr; { ArrAssign(ide, int_of_string i, e) }
  | REPEAT ; c = cmd; FOREVER { Repeat(c) }
  | ide = ID; LPAREN; e = expr ; RPAREN { Call(ide, e) }

declVar:
  | d1 = declVar; SEQ; d2 = declVar { DSeq(d1,d2) } 
  | INT ; ide = ID { IntVar(ide) }
  | ARRAY; ide = ID; LBRACK; dim = CONST; RBRACK {Array (ide, int_of_string dim)}
  | d1 = declVar; SEQ { d1 }
  | { NullVar }

declProc:
  | l = nonempty_list(proc); SEQ { unfoldProcList l }
  | { NullProc }

proc:
  | PROC; ide = ID; LPAREN; par = parFormal; RPAREN; c = cmd; { Proc(ide, par, c) }
 

parFormal:
  | VAL; ide = ID { Val(ide)}
  | REF; ide = ID { Ref(ide)}
