open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else update t x v
        
(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e =
  match e with
  | Value x -> x
  | ID id -> lookup env id
  | Not y -> 
    let t = eval_expr env y in
    (match t with
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError "Expected Boolean Type"))
  | Binop (op, x, y) -> 
    let exp = eval_expr env x in
    let exp2 = eval_expr env y in
    (match exp with
    | Int i -> 
      (match exp2 with
      | Int j -> 
        (match op with
        | Add -> Int (i + j)
        | Sub -> Int (i - j)
        | Mult -> Int (i * j)
        | Div -> if j <> 0 then Int (i / j) else raise DivByZeroError
        | Greater -> Bool (i > j)
        | Less -> Bool (i < j)
        | GreaterEqual -> Bool (i >= j)
        | LessEqual -> Bool (i <= j)
        | Equal -> Bool (i = j)
        | NotEqual -> Bool (i <> j)
        | _ -> raise (TypeError "Invalid operator for int")
        )
      | _ -> raise (TypeError "Expected Int type")
      )
    | Bool b -> 
      (match exp2 with
      | Bool b' -> 
        (match op with
        | Or -> Bool (b || b')
        | And -> Bool (b && b')
        | Equal -> Bool (b = b')
        | NotEqual -> Bool (b <> b')
        | _ -> raise (TypeError "Invalid Operator for bool")
        )
      | _ -> raise (TypeError "Expected Bool type")
      )
    | String s -> 
      (match exp2 with
      | String s' ->
        (match op with
        | Concat -> String (s ^ s')
        | Equal -> Bool (s = s')
        | NotEqual -> Bool (s <> s')
        | _ -> raise (TypeError "Invalid Operator for string")
        )
      | _ -> raise (TypeError "Expected String type")
      )
    | _ -> raise (TypeError "Expected type int, string, bool")
    )
  | If (exp, x, y) -> 
    let exp2 = eval_expr env exp in
    (match exp2 with
    | Bool b -> if b then eval_expr env x else eval_expr env y
    | _ -> raise (TypeError "Expected Bool type"))
  | Let (name, false, value, body) -> 
    let v = eval_expr env value in
    (match v with
    | Int i -> let envir = extend env name (Int i) in 
    eval_expr envir body
    | Bool b -> let envir = extend env name (Bool b) in 
    eval_expr envir body
    | String s -> let envir = extend env name (String s) in 
    eval_expr envir body
    | Closure (x,y,z) -> let envir = extend env name (Closure (x, y, z)) in 
    eval_expr envir body 
    )
  | Let (name, true, value, body) -> 
    let envir = extend_tmp env name in
    let exp = eval_expr envir value in 
    update envir name exp;
    eval_expr envir body 
  | Fun (arg, body) -> Closure (env, arg, body)
  | FunctionCall (exp1, exp2) -> 
    let exp3 = eval_expr env exp1 in
    let exp4 = eval_expr env exp2 in
    (match exp3 with
    | Closure (x, y, z) -> 
      (match exp4 with
      | Int i -> let m = extend x y (Int i) in 
      eval_expr m z
      | Bool b -> let m = extend x y (Bool b) in 
      eval_expr m z
      | String s -> let m = extend x y (String s) in 
      eval_expr m z
      | Closure (x', y', z') -> let m = extend x y (Closure (x', y', z')) in 
      eval_expr m z
      )
    | _ -> raise (TypeError "Expected Closure type")
    )

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with 
  | Def (var, exp) -> let env' = extend_tmp env var in 
  let exp' = eval_expr env' exp in 
  update env' var exp'; 
  env', Some exp'
  | Expr exp -> env, Some (eval_expr env exp)
  | NoOp -> [], None