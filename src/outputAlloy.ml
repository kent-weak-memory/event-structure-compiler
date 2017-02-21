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

exception AlloyOutputException of string

let show_event (E id) =
  if (id + (Char.code 'a') - 1) < (Char.code 'x') then
    Char.escaped (Char.chr (id + (Char.code 'a') - 1))
  else
    String.concat "" ["x"; string_of_int id]

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
  | [] -> raise (AlloyOutputException "invarient violated in alloy output")

let strip_label (L (ev, _)) = ev

let rec print_unrelated fmt f prop vals labs =
  match vals with
  | p::q::xs ->
    let a = ev_with f p labs in
    let b = ev_with f q labs in
    Format.fprintf fmt "   and %s->%s not in MemoryEventStructure.%s\n"
      (show_event (strip_label a)) (show_event (strip_label b)) prop;
    print_unrelated fmt f prop (q::xs) labs
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
let print_alloy fmt alloy_path events labels rels req =
  Format.fprintf fmt "module event_structures/examples\n";
  Format.fprintf fmt "open %s/event_structures/event_structure\n" alloy_path;
  Format.fprintf fmt "open %s/event_structures/configuration\n\n" alloy_path;
  Format.fprintf fmt "pred output { \n";
  Format.fprintf fmt "    some disj %s : E | \n    " (String.concat ", " (List.map show_event events));

  Format.fprintf fmt "   %s" (String.concat "\n   and " (List.map (show_label) labels));

  let order, conflict = rels in
  Format.fprintf fmt "\n\n   and MemoryEventStructure.O = ^(%s" (String.concat " + " (List.map show_relation order));
  Format.fprintf fmt ") + \n    (iden :> (%s))\n\n" (String.concat " + " (List.map show_event events));
  Format.fprintf fmt "   and let ord = MemoryEventStructure.O | \n";
  Format.fprintf fmt "   MemoryEventStructure.C =\n";

  Format.fprintf fmt "       symmClosure[%s" (String.concat " + "
  (List.map (fun (l, r) ->
    Format.sprintf "(%s.ord->%s.ord)" (show_event l) (show_event r)
  ) conflict));

  Format.fprintf fmt "]\n";

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

  let values = find_values labels in
  let sval = List.map (fun l ->
    List.map strip_label (List.filter (same_value l) labels)
  ) values in

  let svalr = List.map (fun x ->
    List.flatten (List.map (fun p ->
        List.map (fun q ->
          (p, q)
        ) x
      ) x
    )
  ) sval in

  let _ = List.map (fun (l,r) ->
    Format.fprintf fmt "   and %s->%s in MemoryEventStructure.loc\n" (show_event l) (show_event r)
  ) (List.flatten slocr) in

  print_unrelated fmt (same_location) "loc" locations labels;

  let _ = List.map (fun (l,r) ->
    Format.fprintf fmt "   and %s->%s in MemoryEventStructure.val\n" (show_event l) (show_event r)
  ) (List.flatten svalr) in

  print_unrelated fmt (same_value) "val" values labels;


  let z = ev_with (same_value) 0 labels in
  Format.fprintf fmt "   and %s in MemoryEventStructure.zero\n" (show_event (strip_label z));

  Format.fprintf fmt "   and some MaxConfig\n";
  Format.fprintf fmt "   and (";
  let rec print_constraints fmt cs =
    match cs with
    | [] -> []
    | c::cs ->
      let r = List.map strip_label c in
      Format.asprintf "((%s) in MaxConfig.c_ev)" (
          String.concat "+" (List.map show_event r)
      ) :: print_constraints fmt cs
  in

  Format.fprintf fmt "%s" ((String.concat "\n    and ") (print_constraints fmt req));
  Format.fprintf fmt ")\n\n";

  Format.fprintf fmt "}\n";

  Format.fprintf fmt "\n\nrun output for %d\n" (List.length events);
  ()
