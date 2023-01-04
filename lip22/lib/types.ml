open Ast
    
type loc = int

type envval = IVar of loc | IArr of loc * int | IProc of parFormal * cmd 
type memval = int

type env = ide -> envval
type mem = loc -> memval

(* The third component of the state is the first free location.
   We assume that the store is unbounded *)
type gamma = Ok | Br
type state = env list * mem * gamma *  loc

let topenv ((el,_,_,_): state) = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

let popenv ((el,_,_,_): state) = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

let getenv ((el,_,_,_): state) = el
let getmem ((_,m,_,_): state) = m

let getgamma((_, _, g, _): state) = g

let getloc ((_,_,_,l): state) = l
  
type conf = St of state | Cmd of cmd * state

exception TypeError of string
exception UnboundVar of ide
