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
