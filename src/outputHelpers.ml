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

open RelateEventStructure
open TranslateLocations
open EventStructure


open Graph
module EventGraph = Imperative.Digraph.Concrete(struct
  type t = RelateEventStructure.event
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end)
open EventGraph
module EventGraphBuilder = Builder.I(EventGraph)
module EventGraphOps = Oper.Make(EventGraphBuilder)

let transitive_reduction edges =
  let g = EventGraph.create () in
  let _ = List.map (fun (l, r) -> EventGraph.add_edge g l r) edges in
  let g = EventGraphOps.transitive_reduction g in
  EventGraph.fold_edges (fun l r acc -> (l, r)::acc) g []

let transitive_closure edges =
  let g = EventGraph.create () in
  let _ = List.map (fun (l, r) -> EventGraph.add_edge g l r) edges in
  let g = EventGraphOps.transitive_closure g in
  EventGraph.fold_edges (fun l r acc -> (l, r)::acc) g []

let remove_reflexive edges =
  let rec rm_reflexive edges xs =
    match edges with
    | (l, r)::ys ->
      if (List.mem (r, l) xs) || (List.mem (l, r) xs) then
        rm_reflexive ys xs
      else
        rm_reflexive ys ((l,r)::xs)
    | [] -> xs
  in
  rm_reflexive edges []

let pick_char id =
  if (id + (Char.code 'a') - 1) < (Char.code 'x') then
    (Char.escaped (Char.chr (id + (Char.code 'a') - 1)))
  else
    "x" ^ (string_of_int id)

let find_loc var_map loc =
  let bindings = VarMap.bindings var_map in
  let (loc_s, _) = List.hd (List.filter (fun (_, li) -> if li == loc then true else false) bindings) in
  loc_s

let same_location l (L (_, es)) =
  match es with
  | Read (_, Loc loc, _) when loc == l -> true
  | Write (_, Loc loc) when loc == l -> true
  | _ -> false

let same_value v (L (_, es)) =
  match es with
  | Read (Val vl, _, _) when vl == v -> true
  | Write (Val vl, _) when vl == v -> true
  | _ -> false

let find_locations labs =
  Mset.make_proper (List.filter ((!=) (-1)) (List.map (fun (L (_, es)) ->
    match es with
    | Read (_, Loc loc, _) -> loc
    | Write (_, Loc loc) -> loc
    | _ -> -1
  ) labs))

let is_init x =
  match x with
  | EventStructure.Init -> true
  | _ -> false

let find_values labs =
  Mset.make_proper (List.filter ((!=) (-1)) (List.map (fun (L (_, es)) ->
    match es with
    | Read (Val vl, _, _) -> vl
    | Write (Val vl, _) -> vl
    | Init -> 0
    | _ -> -1
  ) labs))

let strip_label (L (ev, _)) = ev

let location_eq (Loc a) (Loc b) =
  a == b
