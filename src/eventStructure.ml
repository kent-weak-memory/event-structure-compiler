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
  [@@deriving eq]

type location = Loc of int
  [@@deriving eq]

let pp_location fmt (Loc i) =
  Format.fprintf fmt "%d" i

let pp_value fmt (Val i) =
  Format.fprintf fmt "%d" i

type ev_s =
  | Init
  | Read of (value * location * location)
  | Write of (value * location)
  | Sum of (ev_s * ev_s)  (* + *)
  | Prod of (ev_s * ev_s) (* × *)
  | Comp of (ev_s * ev_s) (* · *)
  | Const of (ev_s * exit_state list)
  | Done
  [@@deriving show, eq]

let pc = ref []
let get_pc () = !pc

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

let find_in v m =
  try
    RegMap.find v m
  with
    Not_found ->
      let _ = Printf.printf "Could not find %d in map.\n" v in
      0

let rec eval_bexp rho l r op =
  let a = eval_exp l rho in
  let b = eval_exp r rho in
  match op with
  | T.Eq -> a == b
  | T.Ne -> a != b
  | T.Gt -> a > b
  | T.Gte -> a >= b
  | T.Lt -> a < b
  | T.Lte -> a <= b
  | T.Assign -> true (* PANIC *)
  | _ -> raise (EventStructureExp ((T.show_op op) ^ " op not implemented. "))
and eval_exp ?(wrn_bexp=true) e rho : int =
  match e with
  | Num n -> n
  | Op (l, op, r) ->
    begin
    match op with
    | T.Plus -> (eval_exp l rho) + (eval_exp r rho)
    | T.Minus -> (eval_exp l rho) - (eval_exp r rho)
    | T.Times -> (eval_exp l rho) * (eval_exp r rho)
    | T.Div -> (eval_exp l rho) / (eval_exp r rho)
    | T.Lt | T.Lte | T.Gt | T.Gte | T.Ne | T.Or | T.And | T.Eq | T.Assign ->
      if wrn_bexp then
      Printf.printf "WARN boolean expression used as value. Up-casting to integer 0|1.\n"
      else ();

      if eval_bexp rho l r op then 1
      else 0
    end
  | Ident i ->
    begin
    match i with
    | Source _ -> raise (EventStructureExp "Unexpected source variable in event structure.")
    | Memory _ -> raise (EventStructureExp "Cannot use memory locations in arithmetic expressions.")
    | Register (_, i) -> find_in i rho
    end
  | Uop _ -> raise (EventStructureExp "Uops not implemented in this context.")

let rec find_constraints (ss: Parser.stmt list) ln =
  match ss with
  (* Strip out line numbers *)
  | LnLoc (s, ln) :: ss ->
    find_constraints (s::ss) ln

  | ExitState (exit_s) :: ss ->
    exit_s :: find_constraints ss ln

  | x::ss ->
    Printf.printf "WARN Throwing away statements out side of parallel composition on line %d.\n" ln;
    pp_stmt Format.std_formatter x;
    find_constraints ss ln
  | [] -> []

(* TODO: I don't think this is convincing. *)
let rec read_ast ?(values=[0;1]) ?(ln=0) ?(rho=RegMap.empty) (ast: Parser.stmt list) =
  match ast with
  | [] -> Done

  | Stmts stmts :: xs ->
    read_ast ~values:values ~ln:ln ~rho:rho (stmts @ xs)

  (* This throws away statements following the PAR. I think this is probably
     desirable under the Jeffrey model *)
  (* TODO: Joining is a place the event structure model should be extended *)
  | Par stmts :: xs ->
    let m = List.map (read_ast ~values:values ~ln:ln ~rho:rho) stmts in
    (* As we don't (currently) expect anything after parallel statements other
       than constraints on exit states, we can assume that all the following
       statements are just exit constraints *)
    Const (prod ln m, find_constraints xs ln)

  (* Strip out Done *)
  | Done :: stmts ->
    read_ast ~values:values ~ln:ln ~rho:rho stmts

  (* Strip out line numbers. *)
  | LnLoc (stmt, ln) :: stmts ->
    read_ast ~values:values ~ln:ln ~rho:rho (stmt::stmts)

  (* Evaluate the expression given the current context to flatten out control *)
  (* Both branches will end up in the event structure because of the map for all
     possible values to be read in the read case *)
  | Ite (e, s1, s2) :: stmts ->
    if (eval_exp ~wrn_bexp:false e rho) == 1 then
      read_ast ~values:values ~ln:ln ~rho:rho (s1::stmts)
    else
      read_ast ~values:values ~ln:ln ~rho:rho (s2::stmts)

  (* Read *)
  | Assign (Register (r, ir), Ident Memory (s, im)) :: stmts ->
    let sums = List.map
      (fun n ->
        Comp (
          Read (Val n, Loc im, Loc ir),
          (read_ast ~values:values ~ln:ln ~rho:(RegMap.add ir n rho) stmts)
        )
      ) values in
    pc := !pc @ List.map (fun n ->
      Read (Val (List.hd values), Loc im, Loc ir),
      Read (Val n, Loc im, Loc ir)
    ) (List.tl values);
    sum ln sums

  | Assign (Register (r, ir), expr) :: stmts ->
    let k = eval_exp expr rho in
    read_ast ~values:values ~ln:ln ~rho:(RegMap.add ir k rho) stmts

  (* Const -> Mem Write *)
  | Assign (Memory (s, im), Num k) :: stmts ->
    Comp (Write (Val k, Loc im), read_ast ~values:values ~ln:ln ~rho:rho stmts)

  | Assign (Memory (s, im), Ident Register (i, ir)) :: stmts ->
    let k = find_in ir rho in
    Comp (Write (Val k, Loc im), read_ast ~values:values ~ln:ln ~rho:rho stmts)

  (* Mem -> Mem write *)
  | Assign (Memory (sl, iml), Ident Memory (sr, imr)) :: stmts ->
    (* we should transform this into a read and a write with a virtual register *)
    let sums = List.map
      (fun n ->
        Comp (
          Read (Val n, Loc imr, Loc iml),
          Comp (
            Write(Val n, Loc iml),
            read_ast ~values:values ~ln:ln ~rho:(RegMap.add imr n rho) stmts
          )
        )
      )
    values in
    pc := !pc @ List.map (fun n ->
      Read (Val (List.hd values), Loc imr, Loc iml),
      Read (Val n, Loc imr, Loc iml)
    ) (List.tl values);
    sum ln sums

  (* We can evaluate expressions too, as long there are no memory locs in expr *)
  | Assign (Memory (s, im), expr) :: stmts ->
    let k = eval_exp expr rho in
    Comp (Write (Val k, Loc im), read_ast ~values:values ~ln:ln ~rho:rho stmts)

  | st ->
    let er = String.concat "\n  " (List.map (Parser.show_stmt) st) in
    raise (EventStructureExp ("bad input on line " ^ string_of_int ln ^ "\n  " ^ er))
