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

let print_alloy fmt events labels rels =
  Format.fprintf fmt "pred output { \n";
  Format.fprintf fmt "    some disj %s : E | \n    " (String.concat ", " (List.map show_event events));

  Format.fprintf fmt "   %s" (String.concat "\n   and " (List.map (show_label) labels));

  let order, conflict = rels in
  Format.fprintf fmt "\n\n   and MemoryEventStrucure.O = ^(%s" (String.concat " + " (List.map show_relation order));
  Format.fprintf fmt ") + \n    (iden :> (%s))\n\n" (String.concat " + " (List.map show_event events));


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
