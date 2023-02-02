%{
open Ast
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


%left OR
%left AND
%nonassoc NOT
%left EQ LEQ
%left PLUS MINUS
%left MUL

%left SEQ 
%left ELSE
%left INT ARRAY PROC 
%left DECVARLSEQ DECLPROCSEQ 


%start <prog> prog

%type <expr> expr
%type <cmd> cmd
%type <declVar> declVar
%type <declProc> declProc
%type <parFormal> parFormal

%%

prog:
  | dV=declVar; dP = declProc; SEQ ; c = cmd; EOF { Prog(dV, dP, c) }
  | dP = declProc; SEQ ; c = cmd; EOF { Prog(NullVar, dP, c) }
  | dV=declVar; c = cmd; EOF { Prog(dV, NullProc, c) }
  | c = cmd; EOF { Prog(NullVar, NullProc, c) }
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
  | ide = ID; LBRACK ; i = CONST; RBRACK; TAKES ; e = expr { ArrAssign(ide, int_of_string i, e) }
  | ide = ID; LPAREN; e = expr ; RPAREN { Call(ide, e) }
  | IF; e0 = expr; THEN; c1 = cmd; ELSE; c2 = cmd; { If(e0,c1,c2) }
  | LBRACE; dv = declVar ; c = cmd; RBRACE { Block(dv, c) } 
  | x = ID; TAKES; e = expr { Assign(x,e) }
  | c1 = cmd; SEQ; c2 = cmd; { Seq(c1,c2) }
  | LPAREN; c = cmd; RPAREN { c }
  | REPEAT ; c = cmd; FOREVER ; { Repeat(c) }

declVar:
  | INT ; ide = ID; SEQ { IntVar(ide) }
  | ARRAY; ide = ID; LBRACK; dim = CONST; RBRACK; SEQ {Array (ide, int_of_string dim)}
  | d1 = declVar; d2 = declVar { DSeq(d1,d2) } %prec DECVARLSEQ

declProc:
  | PROC; ide = ID; LPAREN; par = parFormal; RPAREN; LBRACE; dv = declVar ; c = cmd ;RBRACE { Proc(ide, par, Block(dv, c)) }
  | PROC; ide = ID; LPAREN; par = parFormal; RPAREN; LBRACE; c = cmd ; RBRACE { Proc(ide, par, Block(NullVar, c)) }
  | d1 = declProc; d2 = declProc { DSeqProc(d1,d2) } %prec DECLPROCSEQ

parFormal:
  | VAL; ide = ID { Val(ide)}
  | REF; ide = ID { Ref(ide)}
