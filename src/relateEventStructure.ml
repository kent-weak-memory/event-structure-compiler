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

exception RelateEventStructureException of string

(* ((Order, Conflict), (val, loc, zero)) *)
type ev_r = (ev_s relation * ev_s relation) * (ev_s relation * ev_s relation * ev_s relation)
type event = E of int
type label = L of (event * ev_s)

let event_ordinal = ref 0

let rec read_es es events labels rels =
  let (++) = Mset.union in
  let (><) = Mset.cross in
  match es with
  | Init | Read _ | Write _ ->
    event_ordinal := !event_ordinal + 1;
    let new_ev = E !event_ordinal in
    new_ev::events, (L (new_ev, es))::labels, rels

  | Done -> events, labels, rels

  | Prod (l, r) ->
    let evs_l, labs_l, (ord_l, conf_l) = read_es l events labels rels in
    let evs_r, labs_r, (ord_r, conf_r) = read_es r events labels rels in
    let evs = evs_l ++ evs_r in
    let ord = ord_l ++ ord_r in
    let conf = conf_l ++ conf_r in
    let labs = labs_l ++ labs_r in
    evs, labs, (ord, conf)

  | Sum (l, r) ->
    let evs_l, labs_l, (ord_l, conf_l) = read_es l events labels rels in
    let evs_r, labs_r, (ord_r, conf_r) = read_es r events labels rels in
    let evs = evs_l ++ evs_r in
    let ord = ord_l ++ ord_r in
    let conf = (conf_l ++ conf_r) ++ (evs_l >< evs_r) ++ (evs_r >< evs_l) in
    let labs = labs_l ++ labs_r in
    evs, labs, (ord, conf)

  (* This might not be wrong *)
  | Comp (l, r) ->
    let evs_l, labs_l, (ord_l, conf_l) = read_es l events labels rels in
    let evs_r, labs_r, (ord_r, conf_r) = read_es r events labels rels in

    let labs = labs_l ++ labs_r in
    let evs = (evs_l ++ evs_r) in
    let ord = (evs_l >< evs_r) ++ ord_r ++ ord_l in
    let conf = conf_l ++ conf_r in
    evs, labs, (ord, conf)

  | Const (es, xs) ->
    raise (RelateEventStructureException "Invarient violated: Constraints found in event strucure during relation stage")
