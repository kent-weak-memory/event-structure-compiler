(*
 * Event Structure Compiler
 * Copyright (c) 2016 Simon Cooksey
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


(* Take an AST with virtual locations and convert those into memory and register
 * locations.
 *)

open Parser
module T = Tokeniser

exception UnexpLoc of string

let register_regexp = Str.regexp "r[0-9]+"
let virtual_address = ref 0

(* TODO: This should look up existing items in a map before trying to assign a
new virtual_address *)
let translate_id id =
  match id with
  | Source s ->
    virtual_address := (!virtual_address + 1);
    if Str.string_match register_regexp s 0 then
      Register (Str.matched_string s, !virtual_address)
    else
      Memory (s, !virtual_address)
  | _ -> raise (UnexpLoc "There should be no virtual variables at this stage of the compiler")

let rec translate_expr (e: exp) =
  match e with
  | Ident id -> Ident (translate_id id)
  | Num n -> Num n
  | Op (e1, op, e2) -> Op (translate_expr e1, op, translate_expr e2)
  | Uop (uop, e) -> Uop (uop, translate_expr e)


(* Replaces all the Idents in a given statement with locations which can map to
memory or registers *)
(* TODO: Something's fucky. *)
let rec translate_statement (s: stmt) =
  match s with
  | Assign (i, e) -> Assign (translate_id i, translate_expr e)
  | Ite (e, s1, s2) -> Ite (translate_expr e, translate_statement s1, translate_statement s2)
  | Stmts ss -> Stmts (List.map translate_statement ss)
  | Loc (stmt, ln) -> Loc (translate_statement s, ln)
  | Par (stmts) -> Par (List.map translate_statements stmts)
  | Done -> Done

(* Calls replace_statement for every statement in the ast *)
(* Sag Alloo *)
and translate_statements stmts = List.map translate_statement stmts
