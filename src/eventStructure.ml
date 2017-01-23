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

(*
 * This file translates the source level AST into an event structure AST.
 *
 * r1 = x
 * if (r1 == 0) {
 *   y = 1;
 * } else {
 * }
 *
 * becomes
 *
 * Init . (((Rx0) . (Wy1)) . Done) + ((Rx1) . Done)
 *)

open Parser

exception EventStructureExp of string

type value = Val of int
type location = Loc of int

let pp_location fmt (Loc i) =
  Format.fprintf fmt "%d" i

let pp_value fmt (Val i) =
  Format.fprintf fmt "%d" i

let values = [0; 1]

type ev_s =
  | Init
  | Read of (value * location)
  | Write of (value * location)
  | Sum of (ev_s * ev_s)  (* + *)
  | Prod of (ev_s * ev_s) (* × *)
  | Comp of (ev_s * ev_s) (* · *)
  | Done
  [@@deriving show]

let rec prod ln ev_s =
  match ev_s with
  | [] -> raise (EventStructureExp ("bad input on line " ^ string_of_int ln))
  | [x] -> x
  | x::xs -> Prod(x, prod ln xs)

let rec sum ln ev_s =
  match ev_s with
  | [] -> raise (EventStructureExp ("bad input on line " ^ string_of_int ln))
  | [x] -> x
  | x::xs -> Sum(x, sum ln xs)

let rec cmp ln ev_s =
  match ev_s with
  | [] -> raise (EventStructureExp ("bad input on line " ^ string_of_int ln))
  | [x] -> x
  | x::xs -> Comp(x, cmp ln xs)

let compose () = ()

module RegMap = Map.Make(
  struct type t = int
  let compare = compare end
)

let evaluate_bexp a b op =
  match op with
  | T.Eq -> a == b
  | T.Gt -> a > b
  | T.Lt -> a < b
  | T.Assign -> true (* PANIC *)
  | _ -> raise (EventStructureExp ((T.show_op op) ^ " op not implemented. "))

let find_in v m =
  try
    RegMap.find v m
  with
    Not_found ->
      let _ = Printf.printf "Could not find %d in map.\n" v in
      0

let rec eval_exp rho (e: Parser.exp) =
  match e with
  | Op (Ident (Register (_, ra)), op, Ident (Register (_, rb))) ->
    let va = find_in ra rho in
    let vb = find_in rb rho in
    evaluate_bexp va vb op

  | Op (Ident (Register (_, ra)), op, Num b) ->
    let va = find_in ra rho in
    evaluate_bexp va b op

  | Op (Num a, op, Ident (Register (_, rb))) ->
    let vb = find_in rb rho in
    evaluate_bexp a vb op

  | _ -> raise (EventStructureExp ((Parser.show_exp e) ^ " exp type not implemented."))

(* TODO: I don't think this is convincing. *)
let rec read_ast ?(ln=0) ?(rho=RegMap.empty) (ast: Parser.stmt list) =
  match ast with
  | [] -> Done

  | Stmts stmts :: xs ->
    read_ast ~ln:ln ~rho:rho (stmts @ xs)

  (* This throws away statements following the PAR. I think this is probably
     desirable under the Jeffrey model *)
  (* TODO: Joining is a place the event structure model should be extended *)
  | Par stmts :: xs ->
    let m = List.map (read_ast ~ln:ln ~rho:rho) stmts in
    let _ =
      match xs with
      | [] -> ()
      | _ -> Printf.printf "WARN Throwing away statements out side of parallel composition.\n"
    in
    prod ln m

  (* Strip out Done *)
  (* TODO: Why do we have Done, again? *)
  | Done :: stmts ->
    read_ast ~ln:ln ~rho:rho stmts

  (* Strip out line numbers. *)
  | Loc (stmt, ln) :: stmts ->
    read_ast ~ln:ln ~rho:rho (stmt::stmts)

  (* Evaluate the expression given the current context to flatten out control *)
  (* Both branches will end up in the event structure because of the map for all
     possible values to be read in the read case *)
  | Ite (e, s1, s2) :: stmts ->
    if eval_exp rho e then
      read_ast ~ln:ln ~rho:rho (s1::stmts)
    else
      read_ast ~ln:ln ~rho:rho (s2::stmts)

  (* Read *)
  | Assign (Register (r, ir), Ident Memory (s, im)) :: stmts ->
    let sums = List.map
      (fun n ->
        Comp (
          Read (Val n, Loc im),
          (read_ast ~ln:ln ~rho:(RegMap.add ir n rho) stmts)
        )
      ) values in
    sum ln sums

  (* Const -> Mem Write *)
  | Assign (Memory (s, im), Num k) :: stmts ->
    Comp (Write (Val k, Loc im), read_ast ~ln:ln ~rho:rho stmts)

  (* Mem -> Mem write *)
  | Assign (Memory (sl, iml), Ident Memory (sr, imr)) :: stmts ->
    (* we should transform this into a read and a write with a virtual register *)
    let sums = List.map
      (fun n ->
        Comp (
          Read (Val n, Loc imr),
          Comp (
            Write(Val n, Loc iml),
            read_ast ~ln:ln ~rho:(RegMap.add imr n rho) stmts
          )
        )
      )
    values in
    sum ln sums


  | st ->
    let er = String.concat "\n  " (List.map (Parser.show_stmt) st) in
    raise (EventStructureExp ("bad input on line " ^ string_of_int ln ^ "\n  " ^ er))
