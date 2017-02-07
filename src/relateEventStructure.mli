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

open Relation
open EventStructure
open Parser

(* Order, Conflict, val, loc, zero *)
type ev_r = (ev_s relation * ev_s relation) * (ev_s relation * ev_s relation * ev_s relation)
type event = E of int
type label = L of (event * ev_s)

val read_es :
  ev_s ->
  event list ->
  label list ->
  event relation * event relation ->
  event list * label list * (event relation * event relation)
