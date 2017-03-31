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


exception TikzOutputException of string

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
    raise (TikzOutputException "Event found with no matching label. Labels are incomplete.")

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

let show_relation style long labs var_map (left, right) =
  Format.sprintf "%s -> %s [style=%s]" (show_event long labs var_map left) (show_event long labs var_map right) style

let show_birelation style long labs var_map (left, right) =
  Format.sprintf "%s -> %s [dir=both, style=%s]" (show_event long labs var_map left) (show_event long labs var_map right) style

let print_tikz fmt long var_map test_name events labels rels pc req =
  Format.fprintf fmt "\\RequirePackage{luatex85}\n";
  Format.fprintf fmt "\\documentclass{standalone}\n\n";

  Format.fprintf fmt "\\usepackage{tikz}\n";
  Format.fprintf fmt "\\usepackage{dot2texi}\n";
  Format.fprintf fmt "\\usetikzlibrary{decorations.pathmorphing}\n\n";

  Format.fprintf fmt "\\tikzstyle{basic}=[rectangle,rounded corners=3pt,thin,draw=black,fill=blue!15]\n";
  Format.fprintf fmt "\\tikzstyle{po}=[->,draw=black,line width=1.000]\n";
  Format.fprintf fmt "\\tikzstyle{conf}=[draw=red, decorate, decoration={zigzag,segment length=4,amplitude=.9, post=lineto,post length=2pt}]\n";
  Format.fprintf fmt "\\tikzstyle{just}=[->,draw=green,line width=1.000]\n\n";

  Format.fprintf fmt "\\begin{document}\n";
  Format.fprintf fmt "\\begin{tikzpicture}\n";
	Format.fprintf fmt "  \\begin{dot2tex}[dot,tikz,codeonly,styleonly]\n";

  Format.fprintf fmt "      digraph %s\n" test_name;
  Format.fprintf fmt "      {\n";
  Format.fprintf fmt "        node[style=basic];\n";
  Format.fprintf fmt "        graph[nodesep=\"0\"];\n";
  let order, conflict = rels in
  Format.fprintf fmt "        %s\n" (String.concat "\n        " (List.map (show_relation "po" long labels var_map)
    (transitive_reduction order)));
  Format.fprintf fmt "        %s\n" (String.concat "\n        " (List.map (show_birelation "conf" long labels var_map)
    (remove_reflexive pc)));
  let _ = List.map (fun (l, r) ->
    Format.fprintf fmt "        {rank=same; %s %s}\n"
    (show_event long labels var_map l)
    (show_event long labels var_map r)
  ) pc in
  Format.fprintf fmt "      }\n";

	Format.fprintf fmt "  \\end{dot2tex}\n";
  Format.fprintf fmt "\\end{tikzpicture}%%\n";
  Format.fprintf fmt "\\end{document}\n";

  ()
