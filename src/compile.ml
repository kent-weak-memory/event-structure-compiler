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

(**
 * Compiles Jeffrey style programs into event-structures
 *)


let print_tokens = ref false;;
let filename_ref = ref None;;

let options = Arg.align ([
  ("--print-tokens", Arg.Set print_tokens, " print the tokens as they are tokenised.")
]);;

let usage_msg = "compile.byte MP.jef"

let _ =
  Arg.parse options
    (fun s ->
       match !filename_ref with
       | None ->
         filename_ref := Some s
       | Some s' ->
         (Format.printf "Error: given multiple files to process: %s and %s\n"
            s' s;
          exit 1))
    usage_msg

let filename =
 match !filename_ref with
  | None ->
    (print_string usage_msg;
     exit 1)
  | Some filename ->
    filename;;

let input = Std.input_file filename in
let tokens = Tokeniser.tokenise input 0 0 in

let format_tok (a, _) =
  let fmt = (format_of_string "'%s' ") in
  Printf.printf fmt (Tokeniser.show_token a);
  true
in

let _ = if !print_tokens then
  List.for_all format_tok tokens
  else
    true
in

let _ = Parser.parse_program tokens in

();;
(* let tokens = Tokeniser.tokenise  *)
