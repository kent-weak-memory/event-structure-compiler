(*
 * Event Structure Compiler
 * Copyright (c) 2016 Simon Cooksey
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

open Parser

exception EventStructureExp of string

type value = Val of int
type location = Loc of int

let pp_location fmt (Loc i) =
  Format.fprintf fmt "%d" i

let pp_value fmt (Val i) =
  Format.fprintf fmt "%d" i

let values = [0; 1; 2]

type ev_s =
  | Init
  | Read of (value * location)
  | Write of (value * location)
  | Sum of (ev_s * ev_s)  (* + *)
  | Prod of (ev_s * ev_s) (* × *)
  | Comp of (ev_s * ev_s) (* · *)
  | Done
  [@@deriving show]

let product () = ()

let rec sum ev_s =
  match ev_s with
  | [Read (l, v)] -> Read (l, v)
  | Read (l, v) :: xs ->
    Sum(Read (l, v), sum xs)
  | [Write (l, v)] -> Write (l, v)
  | Write (l, v) :: xs ->
    Sum(Write (l, v), sum xs)
  | _ -> raise (EventStructureExp "bad input")

let rec cmp evs =
  match evs with
  | [] -> raise (EventStructureExp "bad input.")
  | [x] -> x
  | x::xs -> Comp(x, cmp xs)

let compose () = ()

module RegMap = Map.Make(
  struct type t = int
  let compare = compare end
)

(* TODO: I don't think this is convincing. *)
let rec read_ast rho (ast: Parser.stmt list) =
  match ast with
  | [] -> Done

  (* This throws away statements following the PAR. I think this is probably
     desirable? *)
  (* TODO: Warn if xs is not [] ? *)
  (* TODO: Joining is a place the event structure model should be extended *)
  | Par stmts :: xs ->
    let m = List.map (read_ast rho) stmts in
    cmp m (* FIXME: This is not meant to be composition it's meant to be product *)

  (* Strip out Done *)
  (* TODO: Why do we have Done, again? *)
  | Done :: stmts ->
    read_ast rho stmts

  (* Strip out line numbers. We could do something nicer with them,
     but we're not going to. *)
  | Loc (stmt, ln) :: stmts ->
    read_ast rho (stmt::stmts)

  | Ite (e, s1, s2) :: stmts ->
    (* TODO: We need to do something with the expression to model how we get to
       branches. Is this where stuff comes out of rho? *)
    Prod (
      read_ast rho (s1::stmts),
      read_ast rho (s2::stmts)
    )

  (* Read *)
  | Assign (Register (r, ir), Ident Memory (s, im)) :: stmts ->
    let sums = List.map
      (fun n ->
        Comp (
          Read (Val n, Loc im),
          (read_ast (RegMap.add im n rho) stmts)
        )
      ) values in
    sum sums

  (* Write *)
  | Assign (Memory (s, im), Num k) :: stmts ->
    Comp (Write (Val (Int64.to_int k), Loc im), read_ast rho stmts)

  | _ -> raise (EventStructureExp "bad input")
