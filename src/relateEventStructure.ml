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


(* This expands event structures into a list of relations *)

open Parser
open EventStructure
open Relation
open Mset

exception RelateEventStructure of string

(* ((Order, Conflict), (val, loc, zero)) *)
type ev_r = (ev_s relation * ev_s relation) * (ev_s relation * ev_s relation * ev_s relation)

let rec read_es es events rels =
  let (++) = Mset.union in
  let (><) = Mset.cross in
  match es with
  | Init | Read _ | Write _ | Done -> es::events, rels

  | Prod (l, r) ->
    let evs_l, (ord_l, conf_l) = read_es l events rels in
    let evs_r, (ord_r, conf_r) = read_es r events rels in
    let evs = evs_l ++ evs_r in
    let ord = ord_l ++ ord_r in
    let conf = conf_l ++ conf_r in
    evs, (ord, conf)

  | Sum (l, r) ->
    let evs_l, (ord_l, conf_l) = read_es l events rels in
    let evs_r, (ord_r, conf_r) = read_es r events rels in
    let evs = evs_l ++ evs_r in
    let ord = ord_l ++ ord_r in
    let conf = (conf_l ++ conf_r) ++ (evs_l >< evs_r) ++ (evs_r >< evs_l) in
    evs, (ord, conf)

  (* This is wrong *)
  | Comp (l, r) ->
    let evs_l, (ord_l, conf_l) = read_es l events rels in
    let evs_r, (ord_r, conf_r) = read_es r events rels in
    let evs = Mset.union evs_l evs_r in
    let ord = Mset.union ord_l ord_r in
    let conf = Mset.union conf_l conf_r in
    evs, (ord, conf)
