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

let rec parse_expr toks =
  match toks with
  | Tok_Fun::t -> expr toks
  | Tok_Let::t -> expr toks
  | Tok_If::t -> expr toks
  | Tok_LParen::t -> parse_O toks
  | Tok_Not::t -> parse_U toks
  | Tok_Or::t -> expr toks
  | Tok_Int i::t -> parse_O toks
  | Tok_Bool b::t -> parse_O toks
  | Tok_String s::t -> parse_O toks
  | Tok_ID id::t -> parse_O toks
  | _ -> raise (InvalidInputException "expr error")
and expr toks =
  match toks with
  | Tok_Fun::t -> let (t', exp) = parse_F t in (t', exp)
  | Tok_Let::t -> let (t', exp) = parse_L t in (t', exp)
  | Tok_If::t -> let (t', exp) = parse_I t in (t', exp)
  | Tok_Or::t -> let (t', exp) = parse_O t in (t', exp)
  | _ -> raise (InvalidInputException "expr failed")
and parse_L toks =
  match toks with
  | Tok_Rec::Tok_ID v::Tok_Equal::tl -> let (t, exp) = parse_expr tl in
    (match t with 
    | Tok_In::tl' -> let (t', exp2) = parse_expr tl' in 
      (t', Let (v, true, exp, exp2))
    | _ -> raise (InvalidInputException "parse_L failed rec"))
  | Tok_ID v::Tok_Equal::tl -> let (t, exp) = parse_expr tl in
    (match t with
    | Tok_In::tl' -> let (t', exp2) = parse_expr tl' in
      (t', Let (v, false, exp, exp2))
    | _ -> raise (InvalidInputException "parse_L failed no rec"))
  | _ -> raise (InvalidInputException "parse_L failed")
and parse_F toks =
  match toks with
  | Tok_ID v::Tok_Arrow::tl -> let (t, exp) = parse_expr tl in 
    (t, Fun (v, exp))
  | _ -> raise (InvalidInputException "parse_F failed")
and parse_I toks =
  let (t, exp) = parse_expr toks in
  match t with 
  | Tok_Then::tl -> let (t', exp2) = parse_expr tl in
    (match t' with
    | Tok_Else::tl' -> let (t'', exp3) = parse_expr tl' in 
      (t'', If (exp, exp2, exp3))
    | _ -> raise (InvalidInputException "parse_I failed (no else)"))
  | _ -> raise (InvalidInputException "parse_I failed")
    
and parse_O toks = 
  let (t, a) = parse_A toks in
  match lookahead t with 
  | Some Tok_Or -> let t' = match_token t Tok_Or in
    let (t'', o) = parse_O t' in
    (t'', Binop (Or, a, o))
  | Some Tok_And -> parse_A t
  | _ -> t, a
and parse_A toks =
  let (t, e) = parse_E toks in
  match lookahead t with
  | Some Tok_And -> let t' = match_token t Tok_And in
    let (t'', a) = parse_A t' in
    (t'', Binop (And, e, a))
  | Some Tok_Equal -> parse_E t
  | _ -> t, e
and parse_E toks =
  let (t, r) = parse_R toks in
  match lookahead t with
  | Some Tok_Equal -> let t' = match_token t Tok_Equal in
    let (t'', e) = parse_E t' in
    (t'', Binop (Equal, r, e))
  | Some Tok_NotEqual -> let t' = match_token t Tok_NotEqual in
    let (t'', e) = parse_E t' in
    (t'', Binop (NotEqual, r, e))
  | _ -> t, r
and parse_R toks = 
  let (t, s) = parse_S toks in
  match lookahead t with
  | Some Tok_Greater -> let t' = match_token t Tok_Greater in
    let (t'', r) = parse_R t' in
    (t'', Binop (Greater, s, r))
  | Some Tok_Less -> let t' = match_token t Tok_Less in
    let (t'', r) = parse_R t' in
    (t'', Binop (Less, s, r))
  | Some Tok_GreaterEqual -> let t' = match_token t Tok_GreaterEqual in
    let (t'', r) = parse_R t' in
    (t'', Binop (GreaterEqual, s, r))
  | Some Tok_LessEqual -> let t' = match_token t Tok_LessEqual in
    let (t'', r) = parse_R t' in
    (t'', Binop (LessEqual, s, r))
  | _ -> t, s
and parse_S toks = 
  let (t, m) = parse_M toks in 
  match lookahead t with
  | Some Tok_Add -> let t' = match_token t Tok_Add in
    let (t'', s) = parse_S t' in
    (t'', Binop (Add, m, s))
  | Some Tok_Sub -> let t' = match_token t Tok_Sub in
    let (t'', s) = parse_S t' in
    (t'', Binop (Sub, m, s))
  | _ -> t, m
and parse_M toks =
  let (t, c) = parse_C toks in
  match lookahead t with
  | Some Tok_Mult -> let t' = match_token t Tok_Mult in
    let (t'', m) = parse_M t' in
    (t'', Binop (Mult, c, m)) 
  | Some Tok_Div -> let t' = match_token t Tok_Div in
    let (t'', m) = parse_M t' in
    (t'', Binop (Div, c, m))
  | _ -> t, c
and parse_C toks =
  let (t, u) = parse_U toks in
  match lookahead t with
  | Some Tok_Concat -> let t' = match_token t Tok_Concat in
    let (t'', c) = parse_C t' in
    (t'', Binop (Concat, u, c))
  | _ -> t, u
and parse_U toks =
  match toks with
  | Tok_Not::tl -> let (t, exp) = parse_U tl in
    (t, Not exp)
  | _ -> parse_FC toks
and parse_FC toks = 
  let (t, exp) = parse_N toks in
  match t with
  | Tok_Int i ::tl -> (tl, FunctionCall (exp, Value (Int i)))
  | Tok_Bool b ::tl -> (tl, FunctionCall (exp, Value (Bool b)))
  | Tok_String s ::tl -> (tl, FunctionCall (exp, Value (String s)))
  | Tok_ID id ::tl -> (tl, FunctionCall (exp, ID(id)))
  | Tok_LParen::tl -> let (t', exp2) = parse_expr tl in
    (match t' with
    | Tok_RParen::tl' -> (tl', FunctionCall (exp, exp2))
    | _ -> raise (InvalidInputException "parse_FC (paren) error"))
  | _ -> t, exp

and parse_N toks = 
  match lookahead toks with
  | Some Tok_Int(i) -> let t = match_token toks (Tok_Int i) in
    (t, Value(Int i))
  | Some Tok_Bool(b) -> let t = match_token toks (Tok_Bool b) in
    (t, Value (Bool b))
  | Some Tok_String(s) -> let t = match_token toks (Tok_String s) in
    (t, Value (String s))
  | Some Tok_ID(v) -> let t = match_token toks (Tok_ID v) in
    (t, ID (v)) 
  | Some Tok_LParen -> let t = match_token toks Tok_LParen in
    let (t', s) = parse_expr t in
    let t'' = match_token t' Tok_RParen in 
    (t'', s)
  | _ -> raise (InvalidInputException "parse_N failed")

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match toks with
  | Tok_Def::(Tok_ID v)::Tok_Equal::tl -> let (t, exp) = parse_expr tl in
    (match t with
    | [Tok_DoubleSemi] -> ([], (Def (v, exp)))
    | _ -> (t, NoOp))
  | [Tok_DoubleSemi] -> ([], NoOp)
  | _ -> let (t, exp) = parse_expr toks in 
    (match t with
    | [Tok_DoubleSemi] -> ([], (Expr exp))
    | _ -> (t, NoOp)
    )