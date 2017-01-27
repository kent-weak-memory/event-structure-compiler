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
open EventStructure

let show_event (E id) =
  Char.escaped (Char.chr (id + (Char.code 'a') - 1))

let show_label label =
  match label with
  | L (event, Read _) -> String.concat "" [show_event event; " in MemoryEventStructure.R"]
  | L (event, Write _) -> String.concat "" [show_event event; " in MemoryEventStructure.W"]
  | L (event, Init) -> String.concat "" [show_event event; " in MemoryEventStructure.I"]
  | _ -> ""

let show_relation (left, right) =
  String.concat "->" [show_event left; show_event right]

let same_location l (L (_, es)) =
  match es with
  | Read (_, Loc loc) when loc == l -> true
  | Write (_, Loc loc) when loc == l -> true
  | _ -> false

let same_value v (L (_, es)) =
  match es with
  | Read (Val vl, _) when vl == v -> true
  | Write (Val vl, _) when vl == v -> true
  | _ -> false

let find_locations labs =
  Mset.make_proper (List.filter ((!=) (-1)) (List.map (fun (L (_, es)) ->
    match es with
    | Read (_, Loc loc) -> loc
    | Write (_, Loc loc) -> loc
    | _ -> -1
  ) labs))

let find_values labs =
  Mset.make_proper (List.filter ((!=) (-1)) (List.map (fun (L (_, es)) ->
    match es with
    | Read (Val vl, _) -> vl
    | Write (Val vl, _) -> vl
    | _ -> -1
  ) labs))


let relate x xs =
  List.map (fun y -> (x, y)) xs

let strip_label (L (ev, _)) = ev

(* Don't look too hard, eh? *)
let print_alloy fmt events labels rels =
  Format.fprintf fmt "module event_structures/examples\n";
  Format.fprintf fmt "open event_structures/event_structure\n";
  Format.fprintf fmt "open event_structures/configuration\n\n";
  Format.fprintf fmt "pred output { \n";
  Format.fprintf fmt "    some disj %s : E | \n    " (String.concat ", " (List.map show_event events));

  Format.fprintf fmt "   %s" (String.concat "\n   and " (List.map (show_label) labels));

  let order, conflict = rels in
  Format.fprintf fmt "\n\n   and MemoryEventStructure.O = ^(%s" (String.concat " + " (List.map show_relation order));
  Format.fprintf fmt ") + \n    (iden :> (%s))\n\n" (String.concat " + " (List.map show_event events));

  let locations = find_locations labels in
  let sloc = List.map (fun l ->
    List.map strip_label (List.filter (same_location l) labels)
  ) locations in

  let slocr = List.map (fun x ->
    List.flatten (List.map (fun p ->
        List.map (fun q ->
          (p, q)
        ) x
      ) x
    )
  ) sloc in

  let _ = List.map (fun (l,r) ->
    Format.fprintf fmt "   and %s->%s in MemoryEventStructure.loc\n" (show_event l) (show_event r)
  ) (List.flatten slocr) in


  Format.fprintf fmt "   and let ord = MemoryEventStructure.O | \n";
  Format.fprintf fmt "   MemoryEventStructure.C =\n";

  Format.fprintf fmt "       symmClosure[%s" (String.concat " + "
  (List.map (fun (l, r) ->
    Format.sprintf "(%s.ord->%s.ord)" (show_event l) (show_event r)
  ) conflict));

  Format.fprintf fmt "]\n\n";
  Format.fprintf fmt "   and some MaxConfig\n";
  Format.fprintf fmt "}\n";
  ()
