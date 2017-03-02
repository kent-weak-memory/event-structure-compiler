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


exception IsabelleOutputException of string

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
        Format.sprintf "''%s_R%s%d_%s''"
          (Char.escaped (Char.chr (id + (Char.code 'a') - 1)))
          src_loc
          v
          dst_loc
      else
        Format.sprintf "''x%d_R%s%d_%s''" id src_loc v dst_loc
    | L (E eid, Write (Val v, Loc dst)) :: _ when id == eid ->
      let dst_loc = find_loc var_map dst in
      if (id + (Char.code 'a') - 1) < (Char.code 'x') then
        Format.sprintf "''%s_W%s%d''"
          (Char.escaped (Char.chr (id + (Char.code 'a') - 1)))
          dst_loc
          v
      else
        Format.sprintf "''x%d_W%s%d''" id dst_loc v
    | L (E eid, Init) :: _ when id == eid ->
      Format.sprintf "''%s''" (if (id + (Char.code 'a') - 1) < (Char.code 'x') then
        Char.escaped (Char.chr (id + (Char.code 'a') - 1))
      else
        String.concat "" ["x"; string_of_int id])
    | _ :: xs -> show_event long xs var_map (E id)
    | [] ->
      raise (IsabelleOutputException "Event found with no matching label. Labels are incomplete.")
    end
  | false ->
    if (id + (Char.code 'a') - 1) < (Char.code 'x') then
      Format.sprintf "''%s''"
        (Char.escaped (Char.chr (id + (Char.code 'a') - 1)))
    else
      Format.sprintf "''x%d''" id

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

let show_relation long labs var_map (left, right) =
  Format.sprintf "(%s, %s)" (show_event long labs var_map left) (show_event long labs var_map right)

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

let rec ev_with f v labs =
  match labs with
  | x::xs when (f v x) -> x
  | _::xs -> ev_with f v xs
  | [] -> raise (IsabelleOutputException "invarient violated in alloy output")

let strip_label (L (ev, _)) = ev

let rec print_unrelated fmt long f prop var_map vals labs =
  match vals with
  | p::xs ->
    let h = ev_with f p labs in
    let a = List.filter (fun x ->
      let L(_, y) = x in
      not (f p x) &&  not (is_init y)
    ) labs in
    let _ = List.map (fun x ->
      Format.fprintf fmt "    and %s->%s not in MemoryEventStructure.%s\n"
        (show_event long labs var_map (strip_label h))
        (show_event long labs var_map (strip_label x))
        prop
    ) a in
    print_unrelated fmt long f prop var_map xs labs
  | _ -> ()

let location_eq (Loc a) (Loc b) =
  a == b

let rec find_reads_with_dst labels dst =
  match labels with
  | L (ev, Read (v, s, d)) :: xs when location_eq dst d ->
    L (ev, Read (v, s, d)) :: find_reads_with_dst xs dst
  | _ :: xs -> find_reads_with_dst xs dst
  | [] -> []

(* Don't look too hard, eh? *)
let print_isabelle fmt long var_map test_name events labels rels req =
  Format.fprintf fmt "theory %s\n" test_name;
  Format.fprintf fmt "imports EventStructures String\n";
  Format.fprintf fmt "begin\n\n";
  Format.fprintf fmt "definition %s :: \"string event_structure_data\" where\n" test_name;
  Format.fprintf fmt "\"%s ≡ ⦇ \n" test_name;
  Format.fprintf fmt "    event_set = { %s },\n" (String.concat ", " (List.map (show_event long labels var_map) events));

  let order, conflict = rels in
  Format.fprintf fmt "    partial_order =  λx y. (x,y) ∈ { %s },\n" (String.concat ", " (List.map (show_relation long labels var_map) order));
  Format.fprintf fmt "    primitive_conflict =  λx y. (x,y) ∈ { %s },\n" (String.concat ", " (List.map (show_relation long labels var_map) conflict));
  Format.fprintf fmt "    label_function = λx.\n";
  let lab_function_body = List.map (fun (L (event, node)) ->
    Format.sprintf "if x = %s then Label %s" (show_event long labels var_map event) (show_label var_map (L (event, node)))
  ) (List.tl labels) in

  Format.fprintf fmt "        %s\n" (String.concat "\n        else " lab_function_body);
  Format.fprintf fmt "        else Label %s\n" (show_label var_map (List.hd labels));

  Format.fprintf fmt "⦈\"\n\n";
  Format.fprintf fmt "value \"∀ V ∈ { %s } . ∃e∈event_set %s. justifies_event (label_function %s e) (label_function %s V)\""
    (String.concat ", " (List.map (show_event long labels var_map) events)) test_name test_name test_name;

  ()
