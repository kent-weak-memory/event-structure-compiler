(*
 * Event Structure Compiler
 * Copyright (c) 2016 Simon Cooksey
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *)

open List

type 'a edge = 'a * 'a
type 'a relation = ('a edge) list

(* edge_in_relation(e, r) ≜ e ∈ r *)
let rec edge_in_relation e r =
  match r with
  | x::xs when x = e -> true
  | _::xs -> edge_in_relation e xs
  | [] -> false

(* relation_in_relation(r1, r2) ≜ r1 ⊆ r2 *)
let rec relation_in_relation r1 r2 =
  match r1 with
  | x::xs -> (
    match edge_in_relation x r2 with
    | true -> relation_in_relation xs r2
    | false -> false)
  | [] -> true

let relate_idenity (x : 'a) : 'a edge =
  (x, x)

(* reflexive(r, d) ≜ ∀a . a ∈ d → (a, a) ∈ r *)
let is_reflexive r d =
  let iden = List.map relate_idenity d in
  relation_in_relation iden r

(* is_symmetric(r) ≜ ∀(a, b) . (a, b) ∈ r → (b, a) ∈ r *)
let is_symmetric r =
  let rec is_symmetric_inner all remaining =
    match remaining with
    | (a, b)::xs -> (
        match edge_in_relation (b, a) all with
        | true -> is_symmetric_inner r xs
        | false -> false
      )
    | [] -> true
  in
  is_symmetric_inner r r

(* Didn't curry like I wanted it to :( *)
let is_antisymmetric r = not (is_symmetric r)
