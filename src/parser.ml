(*
 * Event Structure Compiler
 * Copyright (c) 2016 Simon Cooksey
 *
 * This portion of the software is derived from Scott Owens' sample compiler.
 *
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

(* Turns a token list into an AST *)

open Str
open List

module T = Tokeniser

exception ParseError of string

type memory_fence = MFence

type location =
  | MemLoc of int
  | RegLoc of int
  [@deriving ord, show]

type id =
  | Source of string
  | Register of string * int
  | Memory of string * int
  [@deriving show]

let show_id id =
  match id with
  | Source s -> s
  | Register (s, i) -> "R" ^ s ^ (string_of_int i)
  | Memory (s, i) -> "_tmp_" ^ s ^ (string_of_int i)

let pp_id fmt id =
  Format.fprintf fmt "%s" (show_id id)

type exp =
  | Ident of id
  | Num of int
  | Op of exp * T.op * exp
  | Uop of T.uop * exp
  [@@deriving show]

let rec show_exp e =
  match e with
  | Ident id -> show_id id
  | Num n -> string_of_int n
  | Op (e1, o, e2) -> (show_exp e1) ^ " " ^ (T.show_op o) ^ " " ^ (show_exp e2)
  | Uop (o, e) -> (T.show_uop o) ^ " " ^ (show_exp e)

let pp_exp fmt e =
  Format.fprintf fmt "%s" (show_exp e)

type exit_state =
  | Allowed of exp
  | Forbidden of exp

let pp_exit_state fmt exit_st =
  match exit_st with
  | Allowed e -> Format.fprintf fmt "expected: %a" pp_exp e
  | Forbidden e -> Format.fprintf fmt "forbidden: %a" pp_exp e

type stmt =
  | Assign of id * exp
  | Ite of exp * stmt * stmt
  | Stmts of stmt list
  | LnLoc of stmt * int (* for line no annotation *)
  | Par of stmt list list
  | ExitState of exit_state
  | Done
  [@@deriving show]

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

let parse_exit_condition (toks : T.tok_loc list) =
  match toks with
  | (T.Allowed, ln) :: toks ->
    let exp, toks = parse_atomic_exp toks in
    (ExitState (Allowed exp), toks)
  | (T.Forbidden, ln) :: toks ->
    let exp, toks = parse_atomic_exp toks in
    (ExitState (Forbidden exp), toks)
  | [] -> raise (ParseError "End of file while parsing exit condition")
  | (_,ln) :: _ -> parse_error ln "Badly formed exit constraint"

let rec parse_stmt toks =
  match toks with
  | [] -> parse_error (-1) "End of file while parsing a statement"
  | (T.Forbidden, ln) :: _ | (T.Allowed, ln) :: _ ->
    let stmt, toks = parse_exit_condition toks in
    (LnLoc (stmt, ln), toks)
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
        (LnLoc (Assign (Source x,  e), ln), toks)
      | _ -> parse_error ln "Expected '=' after identifier"
    )
  | (T.If, ln) :: toks ->
    (match parse_exp toks with
     | (e, toks) ->
      (
        match parse_stmt toks with
        | (s1, (T.Else, _) :: toks) ->
          let (s2, toks) = parse_stmt toks in
          (LnLoc (Ite (e, s1, s2), ln), toks)
        | (s1, _ :: toks) ->
          (LnLoc (Ite (e, s1, Done), ln), toks)
        | (_, _) -> parse_error ln "Bad statement"
      )
    )
  | (T.LCurly, ln) :: toks ->
    let (s_list, toks) = parse_stmt_list toks in
    (LnLoc (Stmts (s_list), ln), toks)
  | (t,ln) :: _ -> parse_error ln ("Bad statement: " ^ (T.show_token t))

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
