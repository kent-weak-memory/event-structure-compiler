(*
 * Event Structure Compiler
 * Copyright (c) 2016 Simon Cooksey
 *
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
open Relation

type value = Val of int
type location = Loc of int

type event =
  | Init
  | Read of (location * value)
  | Write of (location * value)

type eventStructure =
  | Events of event list
  | Seq of event relation
  | Conflict of event relation
  | Justify of event relation

let show_loc l =
  match l with
  | Loc 0 -> "x"
  | Loc 1 -> "y"
  | Loc 2 -> "z"
  | Loc n -> "l" ^ string_of_int n
