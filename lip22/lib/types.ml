open Ast
    
type loc = int

type envval = IVar of loc | IProc of ide * parFormal * cmd 
type memval = int

type env = ide -> envval
type mem = loc -> memval

(* The third component of the state is the first free location.
   We assume that the store is unbounded *)
type state = env list * mem * loc

let topenv ((el,_,_): state) = match el with
    [] -> failwith "empty environment stack"
  | e::_ -> e

let popenv ((el,_,_): state) = match el with
    [] -> failwith "empty environment stack"
  | _::el' -> el'

let getenv ((el,_,_): state) = el
let getmem ((_,m,_): state) = m
let getloc ((_,_,l): state) = l
  
type conf = St of state | Cmd of cmd * state

exception TypeError of string
exception UnboundVar of ide
