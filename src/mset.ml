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

let mem xs a =
  List.mem a xs

let not_mem xs a = not (mem xs a)

let make_proper s =
  let rec make_proper' acc set =
    match set with
    | x::xs when mem acc x -> make_proper' acc xs
    | x::xs -> make_proper' (x::acc) xs
    | [] -> acc
  in
  make_proper' [] s

let union a b =
  make_proper (a @ b)

let intersection a b =
  List.filter (mem a) b

let subset a b =
  List.for_all (mem b) a

let disjoint a b =
  List.for_all (not_mem b) a

let cross l1 l2 =
   List.fold_left
    (fun x a ->
      List.fold_left (fun y b -> (a, b)::y) x l2
    ) [] l1
