open Ast

(* location are just integers *)
type loc = int

(* envval is a value that can be stored in the environment *)
type envval = 
  IVar of loc               (* variable *)
| IArr of loc * int         (* array *)
| IProc of parFormal * cmd  (* procedure *)


(* memval is a value that can be stored in the memory *)
type memval = int

(* env is a function that maps an identifier to an envval *)
type env = ide -> envval

(* mem is a function that maps a location to a memval (int in this case *)
type mem = loc -> memval

(* gamma is used to check if the current command is a break or not *)
type gamma = Ok | Br 

(* state is composed by the environment stack, the memory, the gamma and the current location *)
type state = env list * mem * gamma * loc


(* this function returns enviroment in the top of the stack *)
let topenv ((el,_,_,_): state) = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

(* this function remove and return the enviroment in the top of the stack *)
let popenv ((el,_,_,_): state) = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

(* this function return the enviroment stack *)
let getenv ((el,_,_,_): state) = el
  
(* this function return the memory *)
let getmem ((_,m,_,_): state) = m

(* this function return the gamma *)
let getgamma((_, _, g, _): state) = g

(* this function return the current location *)
let getloc ((_,_,_,l): state) = l

(* type of the configuration *)
type conf = St of state | Cmd of cmd * state


(******************************************************************************)
(*                                 Exceptions                                 *)
(******************************************************************************)
exception TypeError of string   (* type error *)
exception SyntaxError of string (* syntax error *)
exception UnboundVar of string  (* unbound variable *)
exception UnboundMem of int     (* unbound memory location *)
exception NoRuleApplies         (* no rule applies *)
exception IndexOutOfBounds      (* index out of bounds *)
