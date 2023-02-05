open Ast
open Types

(* function that return the value of a variable *)
let apply st x = match (topenv st) x with
    IVar l -> (getmem st) l
  | _ -> raise (TypeError "state not applicable") 
;;

(* function that return the value of an array at index i *)
let apply_array st ide i = match (topenv st) ide with
    IArr (loc, dim) -> if (i < dim) then (getmem st) (loc+i) else raise IndexOutOfBounds
  | _ -> raise (TypeError "state not applicable")

(* parsing function*)
let parse (s : string) : prog =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* binding function *)
let bind f x v = fun y -> if y=x then v else f y


(******************************************************************************)
(*                 Big-step semantics of variable declaration                 *)
(******************************************************************************)
let rec evalDeclVar (e, loc) dv = match dv with
    NullVar -> (e, loc)
  | IntVar(ide) -> let e' = bind e ide (IVar loc) in (e',loc + 1)
  | Array(ide, dim) when (dim > 0) -> let e' = bind e ide (IArr(loc, dim)) in (e',loc + dim)
  | Array(_, _) -> raise (TypeError "Array dimension must be greater than 0")
  | DSeq(d1,d2) -> (let (e',loc') = evalDeclVar (e,loc) d1 in
                    evalDeclVar (e',loc') d2)
;;

(******************************************************************************)
(*                 Big-step semantics of procedure declaration                *)
(******************************************************************************)
let rec evalDeclProc (e, loc) dp = match dp with
    NullProc -> (e, loc)
  | Proc(ide, pf, cmd) -> let e' = bind e ide (IProc (pf, cmd)) in (e',loc)
  | DSeqProc(d1,d2) -> let (e',loc') = evalDeclProc (e,loc) d1 in
    evalDeclProc (e',loc') d2
;;


(******************************************************************************)
(*                      Small-step semantics of expressions                   *)
(******************************************************************************)
let rec trace1_expr st (e : expr) = match e with
    True -> (True,st)
  | False -> (False,st)
  | Var x -> (Const(apply st x), st)  (* x when x is a variable *)
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
  | Arr(ide, Const(i)) -> (Const(apply_array st ide i), st)                 (* a[i] when i is a constant *)
  | Arr(ide, e) -> let (e',st') = trace1_expr st e in (Arr(ide, e'),st')    (* a[e] when e is an expression *)
  | _ -> raise NoRuleApplies  
;;

(* function to check if break is inside a repeat or rep *)
let rec checkBreakIsInsideRep c insideFlag = match c with
  | Break -> insideFlag
  | Seq(c1, c2) -> if (checkBreakIsInsideRep c1 insideFlag) then (checkBreakIsInsideRep c2 insideFlag) else false
  | If(_,c1,c2) -> (checkBreakIsInsideRep c1 insideFlag) && (checkBreakIsInsideRep c2 insideFlag)
  | Repeat(c) -> checkBreakIsInsideRep c true
  | Rep(c) -> checkBreakIsInsideRep c true
  | Block(_,c) -> checkBreakIsInsideRep c insideFlag
  | Bl(c) -> checkBreakIsInsideRep c insideFlag
  | _ -> true
;;

(******************************************************************************)
(*                      Small-step semantics of commands                      *)
(******************************************************************************)
let rec trace1_cmd = function
    St _ -> raise NoRuleApplies 
  | Cmd(c,st)  -> match c with
      Skip -> St st
    | Break -> St (getenv st, getmem st, Br, getloc st) 
    | Assign(x,Const(n)) -> (match topenv st x with                             (* x := n *)
          IVar l -> St (getenv st, bind (getmem st) l n, getgamma st, getloc st)
        | _ -> raise (TypeError "Assign integer to a non-variable"))
    | Assign(x,e) -> let (e',st') = trace1_expr st e in Cmd(Assign(x,e'),st')   (* x := e *)
    | ArrAssign(x, Const(i), Const(n)) -> (match topenv st x with               (* x[i] := n *)
          IArr (loc, dim) when (i < dim) -> St (getenv st, bind (getmem st) (loc + i) n, getgamma st, getloc st)
        | IArr (_, _) -> raise IndexOutOfBounds                                 (* x[i] := n, when i >= dim *)
        | _ -> raise (TypeError "Assign integer to a non-array variable"))      (* x[i] := n, when x is not an array *)
    | ArrAssign(x, e, Const(n)) -> let (e',st') = trace1_expr st e in Cmd(ArrAssign(x, e', Const(n)),st')   (* x[e] := n *)
    | ArrAssign(x, i, e) -> let (e',st') = trace1_expr st e in Cmd(ArrAssign(x, i, e'),st') (* x[i] := e when e is an expression *)
    | Seq(c1, c2) -> (match trace1_cmd (Cmd(c1,st)) with                        (* c1; c2 *)
          St st' when (getgamma st' = Br) -> (St(getenv st', getmem st', Br, getloc st')) (* break*)
        | St st' -> Cmd(c2,st')                                                 (* c1; c2 *)
        | Cmd(c1',st1) -> Cmd(Seq(c1',c2),st1))                                 (* c1'; c2 *)
    | If(True,c1,_) -> Cmd(c1,st)                                               (* if true then c1 else c2 *)
    | If(False,_,c2) -> Cmd(c2,st)                                              (* if false then c1 else c2 *)
    | If(Const(_),_,_) -> raise (TypeError "If with a non-boolean condition")   (* if n then c1 else c2 when n is an integer *)
    | If(e,c1,c2) -> let (e',st') = trace1_expr st e in Cmd(If(e',c1,c2),st')   (* if e then c1 else c2 when e is an expression *)
    | Repeat(c) -> (match trace1_cmd (Cmd(c, st)) with
          St st' when (getgamma st' = Br) -> St (getenv st', getmem st', Ok, getloc st')  (* break in a repeat loop *)
        | St st' -> Cmd(Repeat(c),st')                                          (* repeat c *)
        | Cmd(c',st') -> Cmd(Rep(Seq (c', Repeat(c))),st'))
    | Rep(Repeat c) -> (match trace1_cmd (Cmd(c, st)) with
          St st' when (getgamma st' = Br) -> St (getenv st', getmem st', Ok, getloc st')  (* break in a repeat loop *)
        | St st' -> Cmd(Repeat(c),st')                                          (* repeat c *)
        | Cmd(c',st') -> Cmd(Rep(Seq (c', Repeat(c))),st'))
    | Rep(c) -> (match trace1_cmd (Cmd(c, st)) with
          St st' when (getgamma st' = Br) -> St (getenv st', getmem st', Ok, getloc st')  (* break in a repeat loop *)
        | St st' -> St st'                                                      (* repeat c *)
        | Cmd(c',st') -> Cmd(Rep(c'),st'))                                      (* repeat c' *)             
    | Block(dv, c) -> (match evalDeclVar (topenv st, getloc st) dv with         (* {dv} c *)  
          (e,l) -> Cmd(Bl(c), (e::(getenv st), getmem st, getgamma st, l)))     (* push the new environment *)  
    | Bl(c) -> (match (trace1_cmd(Cmd(c, st))) with                             (* evaluate command inside block until end *)
          St st' -> St (popenv st', getmem st', getgamma st', getloc st')       (* pop the block environment *)
        | Cmd(c', st') -> Cmd(Bl(c'), st'))                                     (* continue evaluating the block until end *)
    | Call (f, Const(n)) -> (match (topenv st) f with                           (* f(n) *)
          IProc((Val x), Block(dv, c)) ->                                       (* procedure with a parameter passed by value*)
          let st' = (bind (topenv st) x (IVar (getloc st))::(getenv st), bind (getmem st) (getloc st) n, getgamma st, (getloc st)+1) in  (* creating a state with parameter defined *)
          let (e,l) = evalDeclVar (topenv st', getloc st') dv in                (* evaluate the declaration variables *)
          Cmd(Bl(c), (e::getenv st, getmem st', getgamma st, l))                (* push the new environment *)
        | _ -> raise (TypeError "Call of a non-procedure")                      (* f(n) when f is not a procedure *)
      )
    | Call (f, Var(x)) -> (match (topenv st) f with
        | IProc((Ref y),Block(dv, c)) -> (match (topenv st) x with              (* procedure with a parameter passed by reference *)
            | IVar l -> (let st' = (bind (topenv st) y (IVar l)::(getenv st), getmem st, getgamma st, getloc st) in   (* creating a state with parameter defined *)
                         let (e,l) = evalDeclVar (topenv st', getloc st') dv in            (* evaluate the declaration variables *)
                         Cmd(Bl(c), (e::getenv st, getmem st', getgamma st, l)))           (* push the new environment *)
            | _ -> raise (TypeError "Reference parameter should be a variable"))(* f(x) when x is not a variable *)
        | IProc((Val _),_) -> let (x',st') = trace1_expr st (Var(x)) in Cmd(Call(f, x'), st') (* if the parameter is passed by value, evaluate it *)
        | _ -> raise (TypeError "Call of a non-procedure")                      (* f(x) when f is not a procedure *)
      )
    | Call (f, e) -> (let (e',st') = trace1_expr st e in Cmd(Call(f, e'),st'))  (* f(e) when e is an expression *)

let rec trace_rec n t =
  if n <= 0 then [t]
  else try
      if (match t with
          | St _ -> true
          | Cmd(c,_) -> checkBreakIsInsideRep c false) then
        (let t' = trace1_cmd t
         in t::(trace_rec (n-1) t'))
      else raise (TypeError "Break is not inside a repeat" )
    with NoRuleApplies -> [t] 

let botenv = fun x -> raise (UnboundVar x)
let botmem = fun l -> raise (UnboundMem l)

(**********************************************************************
   trace : int -> prog -> conf list

   Usage: trace n c performs n steps of the small-step semantics
 **********************************************************************)
let trace n (Prog(dv, dp, c)) =
  if (not(checkBreakIsInsideRep c false)) then raise (TypeError "Break is not inside a repeat")
  else let (e,l) = evalDeclVar (botenv,0) dv
    in let (e', l') = evalDeclProc (e,l) dp 
    in trace_rec n (Cmd(c,([e'], botmem, Ok, l'))) 
