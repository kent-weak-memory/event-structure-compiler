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
open OutputHelpers


exception GraphvizOutputException of string

let rec show_event_long labs var_map (E id) =
  match labs with
  | L (E eid, Read (Val v, Loc src, Loc dst)) :: _ when id = eid ->
    let src_loc = find_loc var_map src in
    let dst_loc = find_loc var_map dst in
    Format.sprintf "\"%s: R%s%d %s\"" (pick_char id) src_loc v dst_loc
  | L (E eid, Write (Val v, Loc dst)) :: _ when id == eid ->
    let dst_loc = find_loc var_map dst in
    Format.sprintf "\"%s: W%s%d\"" (pick_char id) dst_loc v
  | L (E eid, Init) :: _ when id == eid -> "Init"
  | _ :: xs -> show_event_long xs var_map (E id)
  | [] ->
    raise (GraphvizOutputException "Event found with no matching label. Labels are incomplete.")

let show_event long labs var_map (E id) =
  match long with
  | true -> show_event_long labs var_map (E id)
  | false -> Format.sprintf "\"%s\"" (pick_char id)

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
