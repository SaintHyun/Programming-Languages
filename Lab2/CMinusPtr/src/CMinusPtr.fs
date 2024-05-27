module CMinusPtr

open AST
open Types

// Evaluate expression into a value, under the given memory.
let rec evalExp (exp: Exp) (mem: Mem) : Val =
  match exp with
  | Num i -> Int i
  | True -> Bool true
  | False -> Bool false

  | LV lval ->
    match lval with
    | Var str -> 
      if Map.containsKey str mem = true then Map.find str mem
      else raise UndefinedSemantics
    | Deref exp1 ->
      let value = evalExp exp1 mem
      match value with
      | Loc varname -> 
        if Map.containsKey varname mem = true then Map.find varname mem
        else raise UndefinedSemantics
      | _ -> raise UndefinedSemantics

  | AddrOf str ->
    Loc str

  | Add (e1, e2) -> 
    let val1 = evalExp e1 mem
    let val2 = evalExp e2 mem
    match (val1, val2) with
    | (Int i1, Int i2) -> Int (i1 + i2)
    | _ -> raise UndefinedSemantics

  | Sub (e1, e2) -> 
    let val1 = evalExp e1 mem
    let val2 = evalExp e2 mem
    match (val1, val2) with
    | (Int i1, Int i2) -> Int (i1 - i2)
    | _ -> raise UndefinedSemantics

  | LessThan (e1, e2) -> 
    let val1 = evalExp e1 mem
    let val2 = evalExp e2 mem
    match (val1, val2) with
    | (Int i1, Int i2) -> Bool (i1 < i2)
    | _ -> raise UndefinedSemantics

  | GreaterThan (e1, e2) ->
    let val1 = evalExp e1 mem
    let val2 = evalExp e2 mem
    match (val1, val2) with
    | (Int i1, Int i2) -> Bool (i1 > i2)
    | _ -> raise UndefinedSemantics

  | Equal (e1, e2) -> 
    let val1 = evalExp e1 mem
    let val2 = evalExp e2 mem
    match (val1, val2) with
    | (Int i1, Int i2) -> Bool (i1 = i2)
    | (Bool b1, Bool b2) -> Bool (b1 = b2)
    | (Loc l1, Loc l2) -> Bool (l1 = l2)
    | _ -> raise UndefinedSemantics

  | NotEq (e1, e2) ->
    let val1 = evalExp e1 mem
    let val2 = evalExp e2 mem
    match (val1, val2) with
    | (Int i1, Int i2) -> Bool (i1 <> i2)
    | (Bool b1, Bool b2) -> Bool (b1 <> b2)
    | (Loc l1, Loc l2) -> Bool (l1 <> l2)
    | _ -> raise UndefinedSemantics

// Execute a statement and return the updated memory.
let rec exec (stmt: Stmt) (mem: Mem) : Mem =
  match stmt with
  | NOP -> mem // NOP does not change the memory.
  
  | Assign (lval, exp1) ->
    let exp1value = evalExp exp1 mem
    match lval with
    | Var str ->
      let mem2 = Map.add str exp1value mem
      mem2
    | Deref exp2 ->
      let exp2value = evalExp exp2 mem
      match exp2value with
      | Loc varname -> 
        let mem2 = Map.add varname exp1value mem
        mem2
      | _ -> raise UndefinedSemantics

  | Seq (stmt1, stmt2) ->
    let mem2 = exec stmt1 mem
    exec stmt2 mem2

  | If (exp, stmt1, stmt2) ->
    let value = evalExp exp mem
    match value with
    | Bool true -> exec stmt1 mem
    | Bool false -> exec stmt2 mem
    | _ -> raise UndefinedSemantics

  | While (exp, stmt) -> 
    let rec loop (mem: Mem) : Mem =
      let value = evalExp exp mem
      match value with
      | Bool true ->
        let mem2 = exec stmt mem
        loop mem2
      | Bool false -> mem
      | _ -> raise UndefinedSemantics
    loop mem


// The program starts execution with an empty memory. Do NOT fix this function.
let run (prog: Program) : Mem =
  exec prog Map.empty
