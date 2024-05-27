module FMinus

open AST
open Types

// Evaluate expression into a value, under the given environment.

let rec evalExp (exp: Exp) (env: Env) : Val =
  match exp with
  | Num i -> Int i
  | True -> Bool true 
  | False -> Bool false

  | Var str ->
    if Map.containsKey str env = true then Map.find str env
    else raise UndefinedSemantics

  | Neg exp1 ->
    let value1 = evalExp exp1 env
    match value1 with
    | Int i -> Int (-i)
    | _ -> raise UndefinedSemantics
  
  | Add (exp1, exp2) ->
    let (value1, value2) = (evalExp exp1 env, evalExp exp2 env)
    match (value1, value2) with
    | (Int i1, Int i2) -> Int (i1 + i2)
    | _ -> raise UndefinedSemantics

  | Sub (exp1, exp2) ->
    let (value1, value2) = (evalExp exp1 env, evalExp exp2 env)
    match (value1, value2) with
    | (Int i1, Int i2) -> Int (i1 - i2)
    | _ -> raise UndefinedSemantics
  
  | LessThan (exp1, exp2) ->
    let (value1, value2) = (evalExp exp1 env, evalExp exp2 env)
    match (value1, value2) with
    | (Int i1, Int i2) -> Bool (i1 < i2)
    | _ -> raise UndefinedSemantics

  | GreaterThan (exp1, exp2) ->
    let (value1, value2) = (evalExp exp1 env, evalExp exp2 env)
    match (value1, value2) with
    | (Int i1, Int i2) -> Bool (i1 > i2)
    | _ -> raise UndefinedSemantics
  
  | Equal (exp1, exp2) ->
    let (value1, value2) = (evalExp exp1 env, evalExp exp2 env)
    match (value1, value2) with
    | (Int i1, Int i2) -> Bool (i1 = i2)
    | (Bool b1, Bool b2) -> Bool (b1 = b2)
    | _ -> raise UndefinedSemantics

  | NotEq (exp1, exp2) ->
    let (value1, value2) = (evalExp exp1 env, evalExp exp2 env)
    match (value1, value2) with
    | (Int i1, Int i2) -> Bool (i1 <> i2)
    | (Bool b1, Bool b2) -> Bool (b1 <> b2)
    | _ -> raise UndefinedSemantics

  | IfThenElse (exp1, exp2, exp3) ->
    let value1 = evalExp exp1 env
    match value1 with
    | Bool b1 -> if b1 = true then evalExp exp2 env else evalExp exp3 env
    | _ -> raise UndefinedSemantics
  
  | LetIn (str, exp1, exp2) ->
    let value1 = evalExp exp1 env
    let newenv = Map.add str value1 env
    evalExp exp2 newenv

  | LetFunIn (str1, str2, exp1, exp2) ->
    let func = Func (str2, exp1, env)
    let newenv = Map.add str1 func env
    evalExp exp2 newenv

  | LetRecIn (str1, str2, exp1, exp2) ->
    let func = RecFunc (str1, str2, exp1, env)
    let newenv = Map.add str1 func env
    evalExp exp2 newenv

  | Fun (str, exp) ->
    Func (str, exp, env)
  
  | App (exp1, exp2) ->
    let value1 = evalExp exp1 env  
    let value2 = evalExp exp2 env 
    match value1 with
    | Func (str, funcexp, fenv) ->
        let newenv = Map.add str value2 fenv 
        evalExp funcexp newenv 

    | RecFunc (fname, str, funcexp, fenv) ->
        let recenv = Map.add fname (RecFunc (fname, str, funcexp, fenv)) fenv
        let newenv = Map.add str value2 recenv
        evalExp funcexp newenv
        
    | _ -> raise UndefinedSemantics  

// The program starts execution with an empty environment. Do not fix this code.
let run (prog: Program) : Val =
  evalExp prog Map.empty
