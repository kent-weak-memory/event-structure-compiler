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


open List

type 'a edge = 'a * 'a

let pp_edge fmt (a, b) =
  Format.fprintf fmt "%a -> %a" a b

type 'a relation = ('a edge) list
  [@deriving show, eq]

(* edge_in_relation(e, r) ≜ e ∈ r *)
let rec edge_in_relation e r =
  match r with
  | x::xs when x = e -> true
  | _::xs -> edge_in_relation e xs
  | [] -> false

(* relation_in_relation(r1, r2) ≜ r1 ⊆ r2 *)
let rec relation_in_relation r1 r2 =
  match r1 with
  | x::xs -> (
    match edge_in_relation x r2 with
    | true -> relation_in_relation xs r2
    | false -> false)
  | [] -> true

let relate_idenity (x : 'a) : 'a edge =
  (x, x)

(* reflexive(r, d) ≜ ∀a . a ∈ d → (a, a) ∈ r *)
let is_reflexive r d =
  let iden = List.map relate_idenity d in
  relation_in_relation iden r

(* is_symmetric(r) ≜ ∀(a, b) . (a, b) ∈ r → (b, a) ∈ r *)
let is_symmetric r =
  let rec is_symmetric_inner all remaining =
    match remaining with
    | (a, b)::xs -> (
        match edge_in_relation (b, a) all with
        | true -> is_symmetric_inner r xs
        | false -> false
      )
    | [] -> true
  in
  is_symmetric_inner r r

(* Didn't curry like I wanted it to :( *)
let is_antisymmetric r = not (is_symmetric r)
