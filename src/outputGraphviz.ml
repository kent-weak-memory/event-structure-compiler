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


exception GraphvizOutputException of string

let find_loc var_map loc =
  let bindings = VarMap.bindings var_map in
  let (loc_s, _) = List.hd (List.filter (fun (_, li) -> if li == loc then true else false) bindings) in
  loc_s

let rec show_event long labs var_map (E id) =
  match long with
  | true ->
    begin
    match labs with
    | L (E eid, Read (Val v, Loc src, Loc dst)) :: _ when id = eid ->
      let src_loc = find_loc var_map src in
      let dst_loc = find_loc var_map dst in

      if (id + (Char.code 'a') - 1) < (Char.code 'x') then
        Format.sprintf "\"%s: R%s%d %s\""
          (Char.escaped (Char.chr (id + (Char.code 'a') - 1)))
          src_loc
          v
          dst_loc
      else
        Format.sprintf "\"x%d: R%s%d %s\"" id src_loc v dst_loc
    | L (E eid, Write (Val v, Loc dst)) :: _ when id == eid ->
      let dst_loc = find_loc var_map dst in
      if (id + (Char.code 'a') - 1) < (Char.code 'x') then
        Format.sprintf "\"%s: W%s%d\""
          (Char.escaped (Char.chr (id + (Char.code 'a') - 1)))
          dst_loc
          v
      else
        Format.sprintf "\"x%d: W%s%d\"" id dst_loc v
    | L (E eid, Init) :: _ when id == eid -> "Init"
    | _ :: xs -> show_event long xs var_map (E id)
    | [] ->
      raise (GraphvizOutputException "Event found with no matching label. Labels are incomplete.")
    end
  | false ->
    if (id + (Char.code 'a') - 1) < (Char.code 'x') then
      Format.sprintf "\"%s\""
        (Char.escaped (Char.chr (id + (Char.code 'a') - 1)))
    else
      Format.sprintf "\"x%d\"" id

let show_label var_map label =
  match label with
  | L (event, Read (Val v, Loc src, Loc dst)) ->
    let src_loc = find_loc var_map src in
    let dst_loc = find_loc var_map dst in
    Format.sprintf "R ''%s'' %d (* %s *)" src_loc v dst_loc
  | L (event, Write (Val v, Loc dst)) ->
    let dst_loc = find_loc var_map dst in
    Format.sprintf "W ''%s'' %d" dst_loc v
  | L (event, Init) -> "I '''' 0"
  | _ -> ""

let show_relation (label, color) long labs var_map (left, right) =
  Format.sprintf "%s -> %s [label=\"%s\", color=\"%s\"]" (show_event long labs var_map left) (show_event long labs var_map right) label color

let show_birelation (label, color) long labs var_map (left, right) =
  Format.sprintf "%s -> %s [label=\"%s\", color=\"%s\", dir=both]" (show_event long labs var_map left) (show_event long labs var_map right) label color

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

let rec cross xs ys =
  match xs with
  | [] -> []
  | x::xs -> (List.map (fun y -> (x, y)) ys) @ cross xs ys

let build_pc events order conflict =
  List.filter (fun (d, e) ->
    List.mem (d, e) (List.filter (fun (b, c) ->
      List.mem (d, e) conflict &&
      List.mem (c, e) order &&
      List.mem (b, c) conflict &&
      (b == d && c == e)
    ) (cross events events))
  ) (cross events events)

let rec find_reads_with_dst labels dst =
  match labels with
  | L (ev, Read (v, s, d)) :: xs when location_eq dst d ->
    L (ev, Read (v, s, d)) :: find_reads_with_dst xs dst
  | _ :: xs -> find_reads_with_dst xs dst
  | [] -> []

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

(* Don't look too hard, eh? *)
let print_graphviz fmt long var_map test_name events labels rels pc req =
  Format.fprintf fmt "digraph %s\n" test_name;
  Format.fprintf fmt "{\n";
  let order, conflict = rels in
  Format.fprintf fmt "  %s\n" (String.concat "\n  " (List.map (show_relation ("O", "black") long labels var_map)
    (transitive_reduction order)));
  Format.fprintf fmt "  %s\n" (String.concat "\n  " (List.map (show_birelation ("PC", "red") long labels var_map)
    (remove_reflexive pc)));
  Format.fprintf fmt "}";

  ()
