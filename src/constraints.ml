(*
 * Event Structure Compiler
 * Copyright (c) 2017 Simon Cooksey
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

(* This stage of the compile extracts constraints and translates their parts
 * into something consistent with the locations used in the rest of the back
 * end. This translation allows the calculation of event sets which must / must
 * not be in output candidate executions.
 *)

open Parser
open TranslateLocations
open EventStructure
open RelateEventStructure

exception ConstraintException of string

type const = (location * value)
  [@@deriving show]

type const_exp =
| Expected of const list
| Unexpected of const list

let rec extract_constraints ev_s =
  match ev_s with
  | Init | Read _ | Write _ | Done ->
    ev_s, []

  | Prod (l, r) ->
    let l, cl = extract_constraints l in
    let r, cr = extract_constraints r in
    Prod (l, r), (cl @ cr)

  | Sum (l, r) ->
    let l, cl = extract_constraints l in
    let r, cr = extract_constraints r in
    Sum (l, r), (cl @ cr)

  | Comp (l, r)->
    let l, cl = extract_constraints l in
    let r, cr = extract_constraints r in
    Comp (l, r), (cl @ cr)

  | Const (l, exit_s) ->
    let l, cl = extract_constraints l in
    l, (exit_s @ cl)

let rec translate_expression e var_map =
  match e with
  | Op (Ident (Register (rs, _)), T.Eq, Num v) ->
    let loc = VarMap.find rs var_map in
    [(EventStructure.Loc loc, Val v)]
  | Op (left, T.And, right) ->
    translate_expression left var_map @ translate_expression right var_map
  | Op (Ident (_), _, _) ->
    raise (ConstraintException "Constraints must only be on register values")
  | _ ->
    raise (ConstraintException "Malformed constraint")

let rec compile_constraints consts var_map =
  match consts with
  | Allowed e :: xs ->
    Expected (translate_expression e var_map) :: compile_constraints xs var_map
  | Forbidden e :: xs ->
    Unexpected (translate_expression e var_map) :: compile_constraints xs var_map
  | [] -> []

(* TODO: This is naive and assumes that no register is written twice, which is probably untrue *)
let rec find_satisfying labels consts =
  let rec get_my_satisfaction labels const =
    let (Loc l', Val v') = const in
    match labels with
    | L (e, Read(Val v, src, Loc dst)) :: xs when v == v' && l' == dst ->
      L (e, Read(Val v, src, Loc dst)) :: get_my_satisfaction xs const
    | _ :: xs ->  get_my_satisfaction xs const
    | [] -> []
  in

  match consts with
  | (Expected es) :: xs ->
      let candidates = List.map (get_my_satisfaction labels) es in
      candidates @ find_satisfying labels xs
  | (Unexpected es) :: xs ->
      let candidates = List.map (get_my_satisfaction labels) es in
      candidates @ find_satisfying labels xs
  | [] -> []
