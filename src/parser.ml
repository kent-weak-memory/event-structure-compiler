(*
 * Event Structure Compiler
 * Copyright (c) 2016 Simon Cooksey
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *)

open Str
open List

module T = Tokeniser

exception ParseError of string

type memory_fence = MFence

type location =
  | MemLoc of int
  | RegLoc of int
  [@deriving ord]

type id =
  | Source of string
  | Temp of string * int
  [@@deriving ord]

let id_to_string id =
  match id with
  | Source s -> s
  | Temp (s, i) -> "_tmp_" ^ s ^ (string_of_int i)

let pp_id fmt id =
  Format.fprintf fmt "%s" (id_to_string id)

type exp =
  | Ident of id
  | Num of int64
  | Op of exp * T.op * exp
  | Uop of T.uop * exp
  [@@deriving show]

type stmt =
  | Assign of id * exp
  | DoWhile of stmt * exp * stmt
  | Ite of exp * stmt * stmt
  | Stmts of stmt list
  | Loc of stmt * int (* for line no annotation *)
  | Par of stmt list list
  | Done
  [@@deriving show]

module RegisterMap = Map.Make(String)

(* let register_map_foo r m =
  try RegisterMap.find r m
  with
  | Not_found ->
    let v = new_key in
    RegisterMap.add r v m *)

let parse_error (ln : int) (msg : string) : 'a =
  raise (ParseError ("Parse error on line " ^ string_of_int ln ^ ": " ^ msg))

(* Convert the first expression in toks into an AST. Return it with the left
   over tokens. *)
let rec parse_atomic_exp (toks : T.tok_loc list) : exp * T.tok_loc list =
  match toks with
  | [] -> raise (ParseError "End of file while parsing an expression")
  | (T.Ident i, ln) :: toks ->
    (Ident (Source i), toks)
  | (T.Num n, _) :: toks -> (Num n, toks)
  | (T.Uop uop, _) :: toks ->
    let (e, toks) = parse_atomic_exp toks in
    (Uop (uop, e), toks)
  | (T.LParen, ln) :: toks ->
    (match parse_exp toks with
     | (e, (T.RParen, _) :: toks) ->
       (e, toks)
     | _ -> parse_error ln "'(' without matching ')'")
  | (t, ln) :: _ ->
    parse_error ln ("Bad expression (" ^ T.show_token t ^ ")")

and parse_exp (toks : T.tok_loc list) : exp * T.tok_loc list =
  match parse_atomic_exp toks with
  | (e1, (T.Op o, ln) :: toks) ->
    let (e2, toks) = parse_atomic_exp toks in
    (Op (e1, o, e2), toks)
  | (e1, toks) -> (e1, toks)

let rec parse_stmt toks =
  match toks with
  | [] -> parse_error (-1) "End of file while parsing a statement"
  | (T.ParCmp, _) :: (T.LCurly, _) :: toks ->
    let rec parse_stmt_lists (toks) =
      match parse_stmt_list toks with
      | (stmts, (T.LCurly, _) :: toks) ->
        let (next, toks) = parse_stmt_lists toks in
        (stmts :: next, toks)
      | (stmts, toks) ->
        ([stmts], toks)
    in
    let (stmts, toks) = parse_stmt_lists toks in
    (Par stmts, toks)
  | (T.Ident x, ln) :: toks ->
    (
      match toks with
      | (T.Op T.Assign, _) :: toks ->
        let (e, toks) = parse_exp toks in
        (Loc (Assign (Source x,  e), ln), toks)
      | _ -> parse_error ln "Expected '=' after identifier"
    )
  | (T.If, ln) :: toks ->
    (match parse_exp toks with
     | (e, toks) ->
       (match parse_stmt toks with
        | (s1, (T.Else, _) :: toks) ->
          let (s2, toks) = parse_stmt toks in
          (Loc (Ite (e, s1, s2), ln), toks)
        | _ -> parse_error ln "'if' without 'else'"))
  | (T.LCurly, ln) :: toks ->
    let (s_list, toks) = parse_stmt_list toks in
    (Loc (Stmts (s_list), ln), toks)
  | (_,ln) :: _ ->
    parse_error ln "Bad statement"

(* Convert all of the statement in toks into an AST, stopping on a }. Return
   them with the left over tokens *)
and parse_stmt_list (toks) =
  match toks with
  | ((T.RCurly, _) :: toks) -> ([], toks)
  | _ ->
    let (s, toks) = parse_stmt toks in
    let (s_list, toks) = parse_stmt_list toks in
    (s::s_list, toks)


(* Repeatedly parse statments until the input is empty *)
(* NB, the difference between parse_stmt_list which can leave leftover tokens *)
let rec parse_program (toks : T.tok_loc list) : stmt list =
  match toks with
  | [] -> []
  | _ ->
    let (s, toks) = parse_stmt toks in
    let s_list = parse_program toks in
    s::s_list
