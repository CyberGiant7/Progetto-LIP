open Ast
(* open Types *)
      
let string_of_val = function
  | n -> string_of_int n

let rec string_of_expr = function
    True -> "true"
  | False -> "false"
  | Var x -> x
  | Const n -> string_of_int n
  | Not e -> "not " ^ string_of_expr e
  | And(e1,e2) -> string_of_expr e1 ^ " and " ^ string_of_expr e2
  | Or(e1,e2) -> string_of_expr e1 ^ " or " ^ string_of_expr e2
  | Add(e1,e2) -> string_of_expr e1 ^ "+" ^ string_of_expr e2
  | Sub(e1,e2) -> string_of_expr e1 ^ "-" ^ string_of_expr e2
  | Mul(e1,e2) -> string_of_expr e1 ^ "*" ^ string_of_expr e2
  | Eq(e1,e2) -> string_of_expr e1 ^ "=" ^ string_of_expr e2
  | Leq(e1,e2) -> string_of_expr e1 ^ "<=" ^ string_of_expr e2
  | Arr(ide, i) -> ide ^ "[" ^ string_of_int i ^ "]"
  
let rec string_of_declvar = function
  | NullVar -> ""
  | IntVar(x) -> "int " ^ x
  | DSeq(d1,d2) -> string_of_declvar d1 ^ "; " ^ string_of_declvar d2
  | Array(ide, dim) -> "array " ^ ide ^ "[" ^ string_of_int dim ^ "]"

let string_of_parformal = function
    Val e -> "val " ^ string_of_expr e
  | Ref e -> "ref " ^ string_of_expr e

let rec string_of_paractual = function e -> string_of_expr e

and string_of_cmd = function
    Skip -> "skip"
  | Break -> "break"
  | Assign(x,e) -> x ^ ":=" ^ string_of_expr e
  | ArrAssign(ide, i, e) -> ide ^ "[" ^ string_of_int i ^ "]" ^ ":=" ^ string_of_expr e
  | Seq(c1,c2) -> string_of_cmd c1 ^ "; " ^ string_of_cmd c2
  | Repeat(c) -> "repeat " ^ string_of_cmd c ^ " forever"
  | If(e,c1,c2) -> "if " ^ string_of_expr e ^ " then " ^ string_of_cmd c1 ^ " else " ^ string_of_cmd c2
  | Block(dv, c) -> "{" ^ string_of_declvar dv ^ string_of_cmd c ^ "̆}"
  | CallPf(ide, e) -> ide ^ "(" ^ string_of_parformal e ^ ")"
  | CallPa(ide, e) -> ide ^ "(" ^ string_of_paractual e ^ ")"

let rec string_of_declproc = function
  | NullProc -> ""
  | Proc(ide, pf, c) -> "proc " ^ ide ^ "(" ^ string_of_parformal pf ^ ")" ^ "{ " ^ string_of_cmd c ^ " }"
  | DSeqProc(p1,p2) -> string_of_declproc p1 ^ " " ^ string_of_declproc p2

let string_of_prog = function
  | Prog(dv, dp, c) -> string_of_declvar dv ^ " ; " ^ string_of_declproc dp ^ " ; " ^  string_of_cmd c
(* 

let string_of_env1 s x = match topenv s x with
  | IVar l -> string_of_int l ^ "/" ^ x
  | IFun(y,c,e) -> "fun(" ^ y ^ "){" ^ string_of_cmd c ^ "; return " ^ string_of_expr e ^ "}/" ^ x
     
let rec string_of_env s = function
    [] -> ""
  | [x] -> (try string_of_env1 s x with _ -> "")
  | x::dom' -> (try string_of_env1 s x ^ "," ^ string_of_env s dom'
                with _ -> string_of_env s dom')


let string_of_mem1 (m,l) i =
  assert (i<l);
  string_of_val (m i) ^ "/" ^ string_of_int i

let rec range a b = if b<a then [] else a::(range (a+1) b);;

let string_of_mem (m,l) =
  List.fold_left (fun str i -> str ^ (try string_of_mem1 (m,l) i ^ "," with _ -> "")) "" (range 0 (l - 1))

let rec getlocs e = function
    [] -> []
  | x::dom -> try (match e x with
    | IVar l -> l::(getlocs e dom)
    | IFun(_,_,_) -> [])
    with _ -> getlocs e dom
                   
let string_of_state st dom =
  "[" ^ string_of_env st dom ^ "], " ^
  "[" ^ string_of_mem (getmem st,getloc st) ^ "]" ^ ", " ^
  string_of_int (getloc st)

let rec union l1 l2 = match l1 with
    [] -> l2
  | x::l1' -> (if List.mem x l2 then [] else [x]) @ union l1' l2

let rec vars_of_expr = function
    True
  | False
  | Const _ -> []
  | Var x -> [x]
  | Not e -> vars_of_expr e
  | And(e1,e2) 
  | Or(e1,e2) 
  | Add(e1,e2)
  | Sub(e1,e2)
  | Mul(e1,e2)      
  | Eq(e1,e2) 
  | Leq(e1,e2) -> union (vars_of_expr e1) (vars_of_expr e2)
  | Call(f,e) -> union [f] (vars_of_expr e)
  | CallExec(c,e) -> union (vars_of_cmd c) (vars_of_expr e)
  | CallRet(e) -> vars_of_expr e

and vars_of_cmd = function
    Skip -> []
  | Assign(x,e) -> union [x] (vars_of_expr e)
  | Seq(c1,c2) -> union (vars_of_cmd c1) (vars_of_cmd c2)
  | If(e,c1,c2) -> union (vars_of_expr e) (union (vars_of_cmd c1) (vars_of_cmd c2))
  | While(e,c) -> union (vars_of_expr e) (vars_of_cmd c)

let rec vars_of_decl = function
    EmptyDecl -> []
  | IntVar(x) -> [x]
  | Fun(f,x,c,e) -> union [x;f] (union (vars_of_cmd c) (vars_of_expr e))
  | DSeq(d1,d2) -> union (vars_of_decl d1) (vars_of_decl d2)               


let string_of_conf vars = function
    St st -> string_of_state st vars
  | Cmd(c,st) -> "<" ^ string_of_cmd c ^ ", " ^ string_of_state st vars ^ ">"

let rec string_of_trace vars = function
    [] -> ""
  | [x] -> (string_of_conf vars x)
  | x::l -> (string_of_conf vars x) ^ "\n -> " ^ string_of_trace vars l

let rec last = function
    [] -> failwith "last on empty list"
  | [x] -> x
  | _::l -> last l  *)
