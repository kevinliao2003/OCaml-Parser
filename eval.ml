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
(*val eval_expr: environment -> expr -> value*)
let rec eval_expr env e = 
  match e with
  | Value(x) -> x 
  | ID(x) -> lookup env x
  | Not(x) -> let expr = eval_expr env x in
              (match expr with
                | Bool boolean -> Bool(not boolean)
                | _ -> raise (TypeError "Expected type bool"))
  | Binop(op, expr1, expr2) -> let first = eval_expr env expr1 in (*recursively evaluates 1st expression*)
                               let second = eval_expr env expr2 in (*recursively evaluates 2nd expression*)
                               (match op, first, second with
                                | Add, Int(first), Int(second) -> Int(first + second)
                                | Sub, Int(first), Int(second) -> Int(first - second)
                                | Mult, Int(first), Int(second) -> Int(first * second)
                                | Div, Int(first), Int(second) -> if second = 0 
                                                                  then raise (DivByZeroError)
                                                                  else Int(first / second)
                                | Greater, Int(first), Int(second) -> if first > second then Bool true else Bool false
                                | Less, Int(first), Int(second) -> if first < second then Bool true else Bool false
                                | GreaterEqual, Int(first), Int(second) -> if first >= second then Bool true else Bool false
                                | LessEqual, Int(first), Int(second) -> if first <= second then Bool true else Bool false
                                | Concat, String(str1), String(str2) -> String(str1^str2)
                                | Equal, Int(first), Int(second) -> if first = second then Bool true else Bool false
                                | Equal, Bool(boolean1), Bool(boolean2) -> if boolean1 = boolean2 then Bool true else Bool false
                                | Equal, String(str1), String(str2) -> if str1 = str2 then Bool true else Bool false
                                | NotEqual, Int(first), Int(second) -> if first != second then Bool true else Bool false
                                | NotEqual, Bool(boolean1), Bool(boolean2) -> if boolean1 != boolean2 then Bool true else Bool false
                                | NotEqual, String(str1), String(str2) -> if str1 != str2 then Bool true else Bool false
                                | Or, Bool(boolean1), Bool(boolean2) -> if boolean1 = true || boolean2 = true then Bool true else Bool false
                                | And, Bool(boolean1), Bool(boolean2) -> if boolean1 = true && boolean2 = true then Bool true else Bool false
                                | _ -> raise (TypeError "TypeError"))
  | If(guard, truebranch, falsebranch) -> let first = eval_expr env guard in (*recursively evaluates guard*)
                                          (match first with (*checks if guard expression evalues to a Bool*)
                                           | Bool true -> eval_expr env truebranch
                                           | Bool false -> eval_expr env falsebranch
                                           | _ -> raise (TypeError "TypeError"))
  | Let(var, false, init, body) -> let v = eval_expr env init in (*recursively evaluates init*)
                                   let new_env = extend env var v in (*extends environment*)
                                   eval_expr new_env body
  | Let(var, true, init, body) -> let new_env = extend_tmp env var in (*creates temporary environment*)
                                  let v = eval_expr new_env init in (*recursively evaluates init*)
                                  let _ = update new_env var v in (*updates the placeholder to v*)
                                  eval_expr new_env body
  | Fun(var, body) -> Closure(env, var, body)
  | FunctionCall(expr1, expr2) -> let first = eval_expr env expr1 in (*recursively evaluates expr1*)
                                  (match first with
                                  | Closure(env2, var, body) -> let v = eval_expr env expr2 in (*recursively evaluates expr2*)
                                                               let new_var = extend env2 var v in (*updates enviornment*)
                                                               eval_expr new_var body (*recursively evaluates a*)
                                  | _ -> raise (TypeError "TypeError"))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
(*val eval_mutop: environment -> mutop -> environment * value option*)
let eval_mutop env m = 
  match m with
  | Def (var, expr) -> let new_env = extend_tmp env var in (*placeholder*)
                       let v = eval_expr new_env expr in (*evaluates expr*)
                       let _ = update new_env var v in (*updates environment*)
                       (new_env, Some(v))
  | Expr(expr) -> let v = eval_expr env expr in (*evaluates expr*)
                  (env, Some(v))
  | NoOp -> (env, None)