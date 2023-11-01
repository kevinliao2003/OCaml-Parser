open TokenTypes
open Str
open String

(*When parsing "", it should parse to Value(String "")*)

(*helper method for tokenize method*)
let rec tokenize_helper input pos =
  if pos >= length (input) then [] else (*if everything in the input has been processed*)
    if (Str.string_match (Str.regexp "(-[0-9]+)") input pos) then (*Tok_Int of (-)*)
      let matched = Str.matched_string input in
      let number = sub (matched) 1 (length (matched) - 2) in (*without parenthesis*)
      (Tok_Int (int_of_string (number)))::tokenize_helper input (pos + length (matched))
    else if (Str.string_match (Str.regexp "[0-9]+") input pos) then (*Tok_Int of int (+)*)
      let matched = Str.matched_string input in
      (Tok_Int (int_of_string matched))::tokenize_helper input (pos + length (matched))
    else if (Str.string_match (Str.regexp "let") input pos) then
          let first = Str.matched_string input in
          let _ = Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos in
          let second = Str.matched_string input in
          if first = second then (*Tok_rec*)
            (Tok_Let)::tokenize_helper input (pos + 3)
          else 
            (Tok_ID second)::tokenize_helper input (pos + length (second))
    else if (Str.string_match (Str.regexp "rec") input pos) then
          let first = Str.matched_string input in
          let _ = Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos in
          let second = Str.matched_string input in
          if first = second then (*Tok_rec*)
            (Tok_Rec)::tokenize_helper input (pos + 3)
          else 
            (Tok_ID second)::tokenize_helper input (pos + length (second))
    else if (Str.string_match (Str.regexp "in") input pos) then
          let first = Str.matched_string input in
          let _ = Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos in
          let second = Str.matched_string input in
          if first = second then (*Tok_in*)
            (Tok_In)::tokenize_helper input (pos + 2)
          else 
            (Tok_ID second)::tokenize_helper input (pos + length (second))
    else if (Str.string_match (Str.regexp "def") input pos) then
          let first = Str.matched_string input in
          let _ = Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos in
          let second = Str.matched_string input in
          if first = second then (*Tok_def*)
            (Tok_Def)::tokenize_helper input (pos + 3)
          else 
            (Tok_ID second)::tokenize_helper input (pos + length (second))
    else if (Str.string_match (Str.regexp "fun") input pos) then
          let first = Str.matched_string input in
          let _ = Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos in
          let second = Str.matched_string input in
          if first = second then (*Tok_fun*)
            (Tok_Fun)::tokenize_helper input (pos + 3)
          else 
            (Tok_ID second)::tokenize_helper input (pos + length (second))
    else if (Str.string_match (Str.regexp "true") input pos) then
          let first = Str.matched_string input in
          let _ = Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos in
          let second = Str.matched_string input in
          if first = second then (*Tok_true*)
            (Tok_Bool true)::tokenize_helper input (pos + 4)
          else 
            (Tok_ID second)::tokenize_helper input (pos + length (second))
    else if (Str.string_match (Str.regexp "false") input pos) then
          let first = Str.matched_string input in
          let _ = Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos in
          let second = Str.matched_string input in
          if first = second then (*Tok_Bool false*)
            (Tok_Bool false)::tokenize_helper input (pos + 5)
          else 
            (Tok_ID second)::tokenize_helper input (pos + length (second))
    else if (Str.string_match (Str.regexp "if") input pos) then
          let first = Str.matched_string input in
          let _ = Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos in
          let second = Str.matched_string input in
          if first = second then (*Tok_If*)
            (Tok_If)::tokenize_helper input (pos + 2)
          else 
            (Tok_ID second)::tokenize_helper input (pos + length (second))
    else if (Str.string_match (Str.regexp "then") input pos) then
          let first = Str.matched_string input in
          let _ = Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos in
          let second = Str.matched_string input in
          if first = second then (*Tok_Then*)
            (Tok_Then)::tokenize_helper input (pos + 4)
          else 
            (Tok_ID second)::tokenize_helper input (pos + length (second))
    else if (Str.string_match (Str.regexp "else") input pos) then
          let first = Str.matched_string input in
          let _ = Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos in
          let second = Str.matched_string input in
          if first = second then (*Tok_Else*)
            (Tok_Else)::tokenize_helper input (pos + 4)
          else 
            (Tok_ID second)::tokenize_helper input (pos + length (second))
    else if (Str.string_match (Str.regexp "not") input pos) then
          let first = Str.matched_string input in
          let _ = Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos in
          let second = Str.matched_string input in
          if first = second then (*Tok_Not*)
            (Tok_Not)::tokenize_helper input (pos + 3)
          else 
            (Tok_ID second)::tokenize_helper input (pos + length (second))
    else if (Str.string_match (Str.regexp ")") input pos) then (*Tok_RParen*)
      (Tok_RParen)::tokenize_helper input (pos + 1)
    else if (Str.string_match (Str.regexp "(") input pos) then (*Tok_LParen*)
      (Tok_LParen)::tokenize_helper input (pos + 1)
    else if (Str.string_match (Str.regexp "<>") input pos) then (*Tok_NotEqual*)
      (Tok_NotEqual)::tokenize_helper input (pos + 2)
    else if (Str.string_match (Str.regexp "->") input pos) then (*Tok_Arrow*)
      (Tok_Arrow)::tokenize_helper input (pos + 2)
    else if (Str.string_match (Str.regexp ">=") input pos) then (*Tok_GreaterEqual*)
      (Tok_GreaterEqual)::tokenize_helper input (pos + 2)
    else if (Str.string_match (Str.regexp "<=") input pos) then (*Tok_LessEqual*)
      (Tok_LessEqual)::tokenize_helper input (pos + 2)
    else if (Str.string_match (Str.regexp "=") input pos) then (*Tok_Equal*)
      (Tok_Equal)::tokenize_helper input (pos + 1)
    else if (Str.string_match (Str.regexp ">") input pos) then (*Tok_Greater*)
      (Tok_Greater)::tokenize_helper input (pos + 1)
    else if (Str.string_match (Str.regexp "<") input pos) then (*Tok_Less*)
      (Tok_Less)::tokenize_helper input (pos + 1)
    else if (Str.string_match (Str.regexp "||") input pos) then (*Tok_Or*)
      (Tok_Or)::tokenize_helper input (pos + 2)
    else if (Str.string_match (Str.regexp "&&") input pos) then (*Tok_And*)
      (Tok_And)::tokenize_helper input (pos + 2)
    else if (Str.string_match (Str.regexp "(") input pos) then (*Tok_LParen*)
      (Tok_LParen)::tokenize_helper input (pos + 1)
    else if (Str.string_match (Str.regexp "\\+") input pos) then (*Tok_Add*)
      (Tok_Add)::tokenize_helper input (pos + 1)
    else if (Str.string_match (Str.regexp "\\-") input pos) then (*Tok_Sub*)
      (Tok_Sub)::tokenize_helper input (pos + 1)
    else if (Str.string_match (Str.regexp "\\*") input pos) then (*Tok_Mult*)
      (Tok_Mult)::tokenize_helper input (pos + 1)
    else if (Str.string_match (Str.regexp "\\/") input pos) then (*Tok_Div*)
      (Tok_Div)::tokenize_helper input (pos + 1)
    else if (Str.string_match (Str.regexp "\\^") input pos) then (*Tok_Concat*)
      (Tok_Concat)::tokenize_helper input (pos + 1)
    else if (Str.string_match (Str.regexp ";;") input pos) then (*Tok_DoubleSemi*)
      (Tok_DoubleSemi)::tokenize_helper input (pos + 2)
    else if (Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos) then (*Tok_ID*)
      let matched = Str.matched_string input in
      (Tok_ID matched)::tokenize_helper input (pos + length (matched))
    else if (Str.string_match (Str.regexp "\"[^\"]*\"") input pos) then (*Tok_String*)
      let matched = Str.matched_string input in
      let without_quotes = sub (matched) 1 (length (matched) - 2) in (*without quotes*)
      if without_quotes = "" then (*checks if the string is empty*)
          (Tok_String without_quotes)::tokenize_helper input (pos + 2)
      else if (Str.string_match (Str.regexp "\"[^\"]*\"") without_quotes 0) = true then
        failwith "InvalidInputException"
      else
        (Tok_String without_quotes)::tokenize_helper input (pos + length (matched))
    else if (Str.string_match (Str.regexp " ") input pos)
            || (Str.string_match (Str.regexp "\t") input pos) 
            || (Str.string_match (Str.regexp "\n") input pos) then (*whitespace*)
      tokenize_helper input (pos + 1)
    else
      failwith "InvalidInputException"

(*val tokenize : string -> token list*)
let rec tokenize input = 
  tokenize_helper input 0