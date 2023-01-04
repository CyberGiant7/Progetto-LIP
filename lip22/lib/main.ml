open Ast
open Types


exception TypeError of string
exception UnboundVar of string
exception UnboundMem of int
exception NoRuleApplies
exception IndexOutOfBounds

let apply (st : state) x = match topenv st x with
    IVar l -> getmem st l
  | _ -> raise (TypeError "state not aplicable") 
;;

let apply_array (st : state) (ide : ide) (i : int) = match topenv st ide with
    IArr (loc, dim) -> if (i < dim) then getmem st (loc+i) else raise IndexOutOfBounds
  | _ -> raise (TypeError "state not aplicable")

let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
 
(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)



let botenv = fun x -> raise (UnboundVar x)
let botmem = fun l -> raise (UnboundMem l)
    
let bind f x v = fun y -> if y=x then v else f y

let is_val = function
    True -> true
  | False -> true
  | Const _ -> true
  | _ -> false


let rec sem_decl_var (e, loc) (dv : declVar) = match dv with
    NullVar -> (e, loc)
  | IntVar(ide) -> let e' = bind e ide (IVar loc) in (e',loc+1)
  | Array(ide, dim) -> let e' = bind e ide (IArr(loc, dim)) in (e',loc+dim)
  | DSeq(d1,d2) -> let (e',loc') = sem_decl_var (e,loc) d1 in
    sem_decl_var (e',loc') d2
;;

let rec sem_decl_proc (e, loc) (dp : declProc) = match dp with
    NullProc -> (e, loc)
  | Proc(ide, pf, cmd) -> let e' = bind e ide (IProc (pf, cmd)) in (e',loc)
  | DSeqProc(d1,d2) -> let (e',loc') = sem_decl_proc (e,loc) d1 in
  sem_decl_proc (e',loc') d2
;;

let get_iVar_val = function
    IVar l -> l
  | _ -> raise (TypeError "")
;;

let get_iArr_val = function
  IArr (loc, dim) -> (loc, dim)
| _ -> raise (TypeError "")
;;


 let rec trace1_expr st (e : expr) = match e with
    True -> (True,st)
  | False -> (False,st)
  | Var x -> (Const(apply st x), st)
  | Not(True) -> (False,st)
  | Not(False) -> (True,st)
  | Not(e) -> let (e',st') = trace1_expr st e in (Not(e'),st')
  | And(True,e) -> (e,st)
  | And(False,_) -> (False,st)
  | And(e1,e2) -> let (e1',st') = trace1_expr st e1 in (And(e1',e2),st')
  | Or(True,_) -> (True,st)
  | Or(False,e) -> (e,st)
  | Or(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Or(e1',e2),st')
  | Add(Const(n1),Const(n2)) -> (Const(n1+n2),st)
  | Add(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Add(Const(n1),e2'),st')
  | Add(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Add(e1',e2),st')
  | Sub(Const(n1),Const(n2)) -> (Const(n1-n2),st)
  | Sub(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Sub(Const(n1),e2'),st')
  | Sub(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Sub(e1',e2),st')
  | Mul(Const(n1),Const(n2)) -> (Const(n1*n2),st)
  | Mul(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Mul(Const(n1),e2'),st')
  | Mul(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Mul(e1',e2),st')
  | Eq(Const(n1),Const(n2)) -> if n1=n2 then (True,st) else (False,st)
  | Eq(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Eq(Const(n1),e2'),st')
  | Eq(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Eq(e1',e2),st')
  | Leq(Const(n1),Const(n2)) -> if n1<=n2 then (True,st) else (False,st)
  | Leq(Const(n1),e2) -> let (e2',st') = trace1_expr st e2 in (Leq(Const(n1),e2'),st')
  | Leq(e1,e2) -> let (e1',st') = trace1_expr st e1 in (Leq(e1',e2),st')
  | Arr(ide, Const(i)) -> (Const(apply_array st ide i), st)
  | Arr(ide, e) -> let (e',st') = trace1_expr st e in (Arr(ide, e'),st')
  | _ -> raise NoRuleApplies  
;;

let rec trace1_cmd = function
    St _ -> raise NoRuleApplies
  | Cmd(c,st) -> match c with
      Skip -> St st
    | Break -> St (getenv st, getmem st, Br, getloc st)
    | Assign(x,Const(n)) -> (match topenv st x with
        IVar l -> St (getenv st, bind (getmem st) l n, getgamma st, getloc st)
      | _ -> raise (TypeError "Assign to a non-variable"))
    | Assign(x,e) -> let (e',st') = trace1_expr st e in Cmd(Assign(x,e'),st') 
    | ArrAssign(x, i, Const(n)) -> (match topenv st x with
          IArr (loc, dim) when (i < dim) -> St (getenv st, bind (getmem st) (loc + i) n, getgamma st, getloc st)
        | IArr (_, _) -> raise IndexOutOfBounds
        | _ -> raise (TypeError "Assign to a non-variable"))
    | ArrAssign(x, i, e) -> let (e',st') = trace1_expr st e in Cmd(ArrAssign(x, i, e'),st')
    | Seq(c1, c2) -> (match trace1_cmd (Cmd(c1,st)) with
          St st' when (getgamma st' = Br) -> (match c2 with 
            Repeat(_) -> St (getenv st', getmem st', Ok, getloc st')
            | _ -> raise (TypeError "Break outside of a loop"))          
        | St st' -> Cmd(c2,st')
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))
    | If(True,c1,_) -> Cmd(c1,st)
    | If(False,_,c2) -> Cmd(c2,st)
    | If(Const(_),_,_) -> raise (TypeError "If with a non-boolean condition")
    | If(e,c1,c2) -> let (e',st') = trace1_expr st e in Cmd(If(e',c1,c2),st')
    | Repeat(c) -> Cmd(Seq(c, Repeat(c)), st)
    | Block(dv, c) -> (match sem_decl_var (topenv st, getloc st) dv with
        (e,l) -> Cmd(Bl(c), (e::(getenv st), getmem st, getgamma st, l)))
    | Bl(c) -> (match (trace1_cmd(Cmd(c, st))) with
          St st' -> St (popenv st', getmem st', getgamma st', getloc st')
        | Cmd(c', st') -> Cmd(Bl(c'), st'))
    | Call (f, Const(n)) -> (match (topenv st) f with
        IProc((Val x),Block(dv, c)) -> let st' = (bind (topenv st) x (IVar (getloc st))::(getenv st), bind (getmem st) (getloc st) n, Ok, (getloc st)+1) in
        let (e,l) = sem_decl_var (topenv st', getloc st') dv in Cmd(Bl(c), (e::getenv st, getmem st', Ok, l))
          | _ -> raise (TypeError "Call of a non-function") 
          )
    | Call (f, Var(x)) -> (match (topenv st) f with
          IProc((Ref y),c) -> (match (topenv st) x with
            IVar l -> Cmd(c, (bind (topenv st) y (IVar l)::(getenv st), getmem st, Ok, getloc st))
          | _ -> raise (TypeError "Call of a non-function") )
        | IProc((Val _),_) -> let (x',st') = trace1_expr st (Var(x)) in Cmd(Call(f, x'), st')
        | _ -> raise (TypeError "Call of a non-function")
        )
    | Call (f, e) -> (let (e',st') = trace1_expr st e in Cmd(Call(f, e'),st'))

let rec trace_rec n t =
  if n<=0 then [t]
  else try
      let t' = trace1_cmd t
      in t::(trace_rec (n-1) t')
    with NoRuleApplies -> [t] 


(**********************************************************************
 trace : int -> prog -> conf list

 Usage: trace n c performs n steps of the small-step semantics
 **********************************************************************)

 let trace n (Prog(dv, dp, c)) =
  let (e,l) = sem_decl_var (botenv,0) dv
  in let (e', l') = sem_decl_proc (e,l) dp 
in trace_rec n (Cmd(c,([e'],botmem, Ok,l'))) 
