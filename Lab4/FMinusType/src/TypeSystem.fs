namespace FMinus

open AST

exception TypeError

type Type =
  | Int
  | Bool
  | TyVar of string
  | Func of Type * Type

type TypeEnv = Map<string, Type>

module Type =
  let mutable counter = 0
  let TypeVar () =
    counter <- counter + 1
    TyVar (sprintf "'t%d" counter)

  let rec toString (typ: Type): string =
    match typ with
    | Int -> "int"
    | Bool -> "bool"
    | TyVar (s: string) -> s
    | Func (t1: Type, t2) -> sprintf "(%s) -> (%s)" (toString t1) (toString t2)

  let rec gen (typenv: TypeEnv) (exp: Exp) (typ: Type) : (Type * Type) list = 
    match exp with
    | Num _ -> [(typ, Int)]
    | True | False -> [(typ, Bool)]
    | Var str -> 
      if Map.containsKey str typenv then [(typ, (Map.find str typenv))]
      else raise TypeError

    | Neg exp ->
      [(typ, Int)] @ gen typenv exp Int

    | Add (exp1, exp2) | Sub (exp1, exp2) ->
      [(typ, Int)] @ gen typenv exp1 Int @ gen typenv exp2 Int

    | LessThan (exp1, exp2) | GreaterThan (exp1, exp2) ->
      [(typ, Bool)] @ gen typenv exp1 Int @ gen typenv exp2 Int
    
    | Equal (exp1, exp2) | NotEq (exp1, exp2) ->
      let typ1 = TypeVar() 
      [(typ, Bool)] @ gen typenv exp1 typ1 @ gen typenv exp2 typ1

    | IfThenElse (exp1, exp2, exp3) ->
      gen typenv exp1 Bool @ gen typenv exp2 typ @ gen typenv exp3 typ
    
    | LetIn (str, exp1, exp2) ->
      let typ1 = TypeVar()
      let newenv = Map.add str typ1 typenv
      gen typenv exp1 typ1 @ gen newenv exp2 typ
    
    | LetFunIn (str1, str2, exp1, exp2) ->
      let typ1 = TypeVar()
      let typ2 = TypeVar()
      let newenv1 = Map.add str2 typ1 typenv
      let newenv2 = Map.add str1 (Func (typ1, typ2)) typenv
      gen newenv1 exp1 typ2 @ gen newenv2 exp2 typ
    
    | LetRecIn (str1, str2, exp1, exp2) ->
      let typ1 = TypeVar()
      let typ2 = TypeVar()
      let newenv1 = Map.add str1 (Func (typ1 , typ2)) typenv
      let newenv2 =  Map.add str2 typ1 newenv1
      gen newenv2 exp1 typ2 @ gen newenv1 exp2 typ
    
    | Fun (str, exp) ->
      let typ1 = TypeVar()
      let typ2 = TypeVar()
      let newenv = Map.add str typ1 typenv
      [(typ, Func (typ1, typ2))] @ gen newenv exp typ2

    | App (exp1, exp2) ->
      let typ1 = TypeVar()
      gen typenv exp1 (Func (typ1, typ)) @ gen typenv exp2 typ1

  let rec apply (typ: Type) (env: TypeEnv) : Type = 
    match typ with
    | Int -> Int
    | Bool -> Bool
    
    | TyVar str ->
      if Map.containsKey str env then Map.find str env
      else TyVar str

    | Func (typ1, typ2) ->
      let app1 = apply typ1 env
      let app2 = apply typ2 env
      Func (app1, app2)

  let rec unify (equation : (Type * Type)) (env: TypeEnv) : TypeEnv = 
    match equation with
    | (typ1, typ2) -> 
      let app1 = apply typ1 env
      let app2 = apply typ2 env 
      extend (app1, app2) env

  and extend (equation: (Type * Type)) (env: TypeEnv) : TypeEnv = 
    match equation with
    | (Int, Int) | (Bool, Bool) -> env

    | (Func (tx1, ty1), Func (tx2, ty2)) ->
      let newenv = (extend (tx1, tx2) env)
      unify (ty1, ty2) newenv

    | (TyVar typ, t) | (t, TyVar typ) ->
      if t = TyVar typ then env
      else if occur (TyVar typ) t then raise TypeError
      else Map.add typ t (Map.map (fun key value -> apply value (Map.add typ t env)) env)

    | _ -> raise TypeError

  and occur (typ: Type) (t: Type) : bool = 
    match t with
    | TyVar x -> typ = TyVar x
    | Func (t1, t2) -> occur typ t1 || occur typ t2
    | _ -> false

  let rec solving (equation: (Type * Type) list) (env: TypeEnv) (typ: Type) : Type = 
    match equation with
    | [] -> 
      match typ with
      | TyVar t -> 
        if Map.containsKey t env then Map.find t env
        else raise TypeError

      | _ -> raise TypeError

    | (typ1, typ2) :: tail ->
      let newenv = unify (typ1, typ2) env
      solving tail newenv typ

  let infer (prog: Program) : Type =
    let typ = TypeVar()
    let equation = gen Map.empty prog typ
    solving equation Map.empty typ
