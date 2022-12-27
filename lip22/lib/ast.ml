type ide = string
  
type expr =
  | True
  | False
  | Var of ide
  | Const of int
  | Not of expr
  | And of expr * expr
  | Or of expr * expr
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Eq of expr * expr
  | Leq of expr * expr
  | Arr of ide * int

type parFormal =
    Val of int
  | Ref of int

type declVar =
  | NullVar
  | DSeq of declVar * declVar
  | IntVar of ide 
  | Array of ide * int

type cmd =
  | Skip
  | Break
  | Assign of ide * expr
  | ArrAssign of ide * int * expr
  | Seq of cmd * cmd
  | Repeat of cmd
  | If of expr * cmd * cmd
  | Block of declVar * cmd

type declProc = 
  | NullProc
  | DSeqProc of declProc * declProc
  | Proc of ide * parFormal * cmd

type prog = Prog of declVar * declProc * cmd
