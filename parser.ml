open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

(*token list -> token list * expr*)
(*parse_expr [Tok_Int(1); Tok_Add; Tok_Int(2)] =  ([], Binop (Add, Value (Int 1), Value (Int 2)))
  parse_expr [Tok_Int(1)] = ([], Value (Int 1))*)
let rec parse_expr toks = 
  let token = lookahead toks in (*first element in the list*)
  match token with
  | (Some Tok_Let) -> parse_Let toks 
  | (Some Tok_If) -> parse_If toks
  | (Some Tok_Fun) -> parse_Function toks
  | _ -> parse_Or toks

(*Parses LetExpr*)
and parse_Let toks =
  let one = match_token toks Tok_Let in
  let token = lookahead one in (*checks for Tok_Rec*)
  match token with
  | (Some Tok_Rec) -> let two = match_token one Tok_Rec in (*removes Tok_Rec*)
                      let str = (match lookahead two with (*extracts string for Tok_ID*)
                                   | (Some (Tok_ID s)) -> s 
                                   | _ -> raise (InvalidInputException "InvalidInputException") ) in
                      let three = match_token two (Tok_ID (str)) in (*checks for Tok_ID*)
                      let four = match_token three Tok_Equal in (*checks for Tok_Equal*)
                      let (five, expr) = parse_expr four in (*recurive call to parse expression*)
                      let remove_in = match_token five Tok_In in (*removes Tok_In*)
                      let (six, expr2) = parse_expr remove_in in (*recursive call to parse expression*)
                      (six, Let(str, true, expr, expr2))
  | _ -> let str = (match lookahead one with (*extracts string for Tok_ID*)
                    | (Some (Tok_ID s)) -> s 
                    | _ -> raise (InvalidInputException "InvalidInputException") ) in
         let two = match_token one (Tok_ID (str)) in (*checks for Tok_ID*)
         let three = match_token two Tok_Equal in (*checks for Tok_Equal*)
         let (four, expr) = parse_expr three in (*recurive call to parse expression*)
         let remove_in = match_token four Tok_In in (*removes Tok_In*)
         let (five, expr2) = parse_expr remove_in in (*recursive call to parse expression*)
         (five, Let(str, false, expr, expr2))

(*Parses FunctionExpr*)
and parse_Function toks =
  let one = match_token toks Tok_Fun in
  let token = lookahead one in (*checks for Tok_ID*)
  match token with
  | (Some Tok_ID (str)) -> let two = match_token one (Tok_ID (str)) in (*removes Tok_ID*)
                           let three = match_token two Tok_Arrow in (*removes Tok_Arrow*)
                           let (four, expr) = parse_expr three in (*recursive call to parse expression*)
                           (four, Fun(str, expr))
  | _ -> raise (InvalidInputException "InvalidInputException") 

(*Parses IfExpr*)
and parse_If toks =
  let one = match_token toks Tok_If in (*removes Tok_If*)
  let (two, expr) = parse_expr one in (*recursive call to parse expression*)
  let three = match_token two Tok_Then in (*removes Tok_Then*)
  let (four, expr2) = parse_expr three in (*recursive call to parse expression*)
  let five = match_token four Tok_Else in (*removes Tok_Else*)
  let (six, expr3) = parse_expr five in (*recursive call to parse expression*)
  (six, If(expr, expr2, expr3))

(*Parses OrExpr*)
and parse_Or toks =
  let (one, expr) = parse_And toks in
  match lookahead one with
  | (Some Tok_Or) -> let two = match_token one Tok_Or in
                     let (three, expr2) = parse_Or two in
                     (three, Binop(Or, expr, expr2)) 
  | _ -> one, expr

(*Parses AndExpr*)
and parse_And toks =
  let (one, expr) = parse_Equality toks in
  match lookahead one with
  | (Some Tok_And) -> let two = match_token one Tok_And in
                      let (three, expr2) = parse_And two in
                      (three, Binop(And, expr, expr2))
  | _ -> one, expr

(*Parses EqualityExpr*)
and parse_Equality toks =
  let (one, expr) = parse_Relational toks in
  match lookahead one with
  | (Some Tok_Equal) -> let two = match_token one Tok_Equal in
                        let (three, expr2) = parse_Equality two in
                        (three, Binop(Equal, expr, expr2))
  | (Some Tok_NotEqual) -> let two = match_token one Tok_NotEqual in
                           let (three, expr2) = parse_Equality two in
                           (three, Binop(NotEqual, expr, expr2))
  | _ -> one, expr

(*Parses RelationalExpr*)
and parse_Relational toks = 
  let (one, expr) = parse_Additive toks in
  match lookahead one with
  | (Some Tok_Less) -> let two = match_token one Tok_Less in
                       let (three, expr2) = parse_Relational two in
                       (three, Binop(Less, expr, expr2))
  | (Some Tok_Greater) -> let two = match_token one Tok_Greater in
                          let (three, expr2) = parse_Relational two in
                          (three, Binop(Greater, expr, expr2))
  | (Some Tok_LessEqual) -> let two = match_token one Tok_LessEqual in
                            let (three, expr2) = parse_Relational two in
                            (three, Binop(LessEqual, expr, expr2))
  | (Some Tok_GreaterEqual) -> let two = match_token one Tok_GreaterEqual in
                               let (three, expr2) = parse_Relational two in
                               (three, Binop(Greater, expr, expr2))
  | _ -> one, expr

(*Parses Additive*)
and parse_Additive toks =
  let (one, expr) = parse_Multiplicative toks in
  match lookahead one with
  | (Some Tok_Add) -> let two = match_token one Tok_Add in
               let (three, expr2) = parse_Additive two in
               (three, Binop(Add, expr, expr2))
  | (Some Tok_Sub) -> let two = match_token one Tok_Sub in
               let (three, expr2) = parse_Additive two in
               (three, Binop(Sub, expr, expr2))
  | _ -> one, expr

(*Parses Multiplicative*)
and parse_Multiplicative toks = 
  let (one, expr) = parse_Concat toks in
  match lookahead one with
  | (Some Tok_Mult) -> let two = match_token one Tok_Mult in
                       let (three, expr2) = parse_Multiplicative two in
                       (three, Binop(Mult, expr, expr2))
  | (Some Tok_Div) -> let two = match_token one Tok_Div in
                      let (three, expr2) = parse_Multiplicative two in
                      (three, Binop(Div, expr, expr2))
  | _ -> one, expr

(*Parses ConCat*)
and parse_Concat toks =
  let (one, expr) = parse_Unary toks in
  match lookahead one with
  | (Some Tok_Concat) -> let two = match_token one Tok_Concat in
                         let (three, expr2) = parse_Concat two in
                         (three, Binop(Concat, expr, expr2))
  | _ -> one, expr

(*Parses Unary*)
and parse_Unary toks =
  match lookahead toks with
  | (Some Tok_Not) -> let one = match_token toks Tok_Not in
                      let (two, expr) = parse_Concat one in
                      (two, Not(expr))
  | _ -> parse_FunctionCall toks

(*Parses FunctionCall*)
and parse_FunctionCall toks = 
  let (one, expr) = parse_Primary toks in
  match lookahead one with
  | (Some Tok_Int (integer)) -> let (two, expr2) = parse_Primary one in
                                (two, FunctionCall(expr, expr2))
  | (Some Tok_Bool (boolean)) -> let (two, expr2) = parse_Primary one in
                                 (two, FunctionCall(expr, expr2))
  | (Some Tok_String (str)) -> let (two, expr2) = parse_Primary one in
                               (two, FunctionCall(expr, expr2))
  | (Some Tok_ID (str)) -> let (two, expr2) = parse_Primary one in
                           (two, FunctionCall(expr, expr2))
  | (Some Tok_LParen) -> let (two, expr2) = parse_Primary one in
                         (two, FunctionCall(expr, expr2))
  | _ -> one, expr

(*Parses Primary*)
and parse_Primary toks = 
  match lookahead toks with
  | (Some Tok_Int (integer)) -> let two = match_token toks (Tok_Int (integer)) in
                                (two, Value(Int(integer)))
  | (Some Tok_Bool (boolean)) -> let two = match_token toks (Tok_Bool (boolean)) in
                                 (two, Value(Bool(boolean)))
  | (Some Tok_String (str)) -> let two = match_token toks (Tok_String (str)) in
                               (two, Value(String(str)))
  | (Some Tok_ID (str)) -> let two = match_token toks (Tok_ID (str)) in
                           (two, ID(str))
  | (Some Tok_LParen) -> let two = match_token toks Tok_LParen in (*( Expr )*)
                         let (three, expr2) = parse_expr two in
                         let four = match_token three Tok_RParen in
                         (four, expr2)
  | _ -> raise (InvalidInputException "InvalidInputException")

(* Part 3: Parsing mutop *)

(*type mutop = 
  | Def of var * expr
  | Expr of expr
  | NoOp*)

(*Mutop -> DefMutop | ExprMutop | ;;
  DefMutop -> def Tok_ID = Expr ;;
  ExprMutop -> Expr ;;*)

(*val parse_mutop : token list -> (token list * mutop)*)
let rec parse_mutop toks = 
  let token = lookahead toks in
  match token with
  | (Some Tok_Def) -> parse_DefMutop toks
  | (Some Tok_DoubleSemi) -> ([], NoOp)
  | _ -> parse_ExprMutop toks (*all other expressions*)

and parse_DefMutop toks =
  let first = match_token toks Tok_Def in (*removes Tok_Def*)
  match lookahead first with (*checks for Tok_ID*)
  | (Some Tok_ID (str)) -> let second = match_token first (Tok_ID (str)) in (*removes Tok_ID*)
                           let third = match_token second (Tok_Equal) in (*removes Tok_Equal*)
                           let (fourth, expr) = parse_expr third in (*recursive call to parse expression*)
                           let fifth = match_token fourth Tok_DoubleSemi in (*removes Tok_DoubleSemi*)
                           (fifth, Def(str, expr))
  | _ -> raise (InvalidInputException "InvalidInputException")

and parse_ExprMutop toks =
  let (first, expr) = parse_expr toks in (*recursive call to parse expression*)
  let second = match_token first Tok_DoubleSemi in (*removes Tok_DoubleSemi*)
  (second, Expr(expr))