open RelateEventStructure
open TranslateLocations
open EventStructure
open OutputHelpers

exception RaduSimulatorExecption of string

let rec get_reads labels =
  match labels with
  | L (E ev, Read (Val v, Loc src, Loc dst)) as r :: xs ->
    (r :: get_reads xs)
  | _ :: xs -> get_reads xs
  | [] -> []

let rec get_justifies read labels =
  match read with
  | L (E id, Read (Val v, Loc l, _)) ->
    begin
    match labels with
    | L (E id2, Write (Val v2, Loc l2)) :: xs when v == v2 && l = l2 ->
      (E id2, E id) :: get_justifies read xs
    | L (E id2, Init) :: xs when v == 0 ->
      (E id2, E id) :: get_justifies read xs
    | _ :: xs -> get_justifies read xs
    | [] -> []
    end
  | _ -> raise (RaduSimulatorExecption "Can't justify things which aren't reads!")

let rec get_all_justifies reads labels =
  match reads with
  | [] -> []
  | x::xs -> get_justifies x labels :: get_all_justifies xs labels


let show_event (E id) = string_of_int id

let show_relation (left, right) =
  Format.sprintf "%s %s" (show_event left) (show_event right)

(* Print a list of (event id, label) pairs. *)
let rec print_labels fmt labels var_map =
  match labels with
  | L (E eid, Read (Val value, Loc src, Loc dst)) :: xs ->
    let src_loc = find_loc var_map src in
    let dst_loc = find_loc var_map dst in
    Format.fprintf fmt "  %d \"R%s%d %s\"\n" eid src_loc value dst_loc;
    print_labels fmt xs var_map
  | L (E eid, Write (Val value, Loc dst)) :: xs ->
    let dst_loc = find_loc var_map dst in
    Format.fprintf fmt "  %d \"W%s%d\"\n" eid dst_loc value;
    print_labels fmt xs var_map
  | L (E eid, Init) :: xs ->
    Format.fprintf fmt "  %d \"Init\"\n" eid;
    print_labels fmt xs var_map
  | _ :: _ -> raise (RaduSimulatorExecption "Unlabelable event.")
  | [] -> ()

let rec print_justifies fmt justifies =
  match justifies with
  | [] -> ()
  | j :: js ->
    let r = List.map show_relation j in
    Format.fprintf fmt "  %s\n" (String.concat "\n  " r);
    print_justifies fmt js

let rec get_preds order ev =
  let rec get_preds_i order_n =
    match order_n with
    | (e1, e2) :: orders when ev == e2 ->
        e1 :: (get_preds order e1) @ (get_preds_i orders)
    | _ :: orders -> get_preds_i orders
    | [] -> [ev]
  in
  get_preds_i order

let rec get_succs order ev =
  let rec get_succs_i order_n =
    match order_n with
    | (e1, e2) :: orders when ev == e1 ->
        e1 :: e2 :: (get_succs order e2) @ (get_succs_i orders)
    | _ :: orders -> get_succs_i orders
    | [] -> [ev]
  in
  get_succs_i order

let build_exec evs order =
  let preds = List.fold_left (@) [] (List.map (get_preds order) evs) in
  (* let succs = List.fold_left (@) [] (List.map (get_succs order) evs) in *)
  (* Mset.union succs preds *)
  Mset.make_proper preds

let longest a b =
  if List.length a > List.length b then a else b

let print_sim fmt long var_map test_name events labels rels pc (expected_labels, forbidden_labels) =
  Format.fprintf fmt "events %d\n" (List.length events);
  let reads = get_reads labels in
  Format.fprintf fmt "reads %s\n" (String.concat " " (List.map (fun f -> show_event (strip_label f)) reads));

  let order, conflict = rels in

  Format.fprintf fmt "labels\n";
  print_labels fmt labels var_map;

  Format.fprintf fmt "justifies\n";
  print_justifies fmt (get_all_justifies reads labels);

  Format.fprintf fmt "conflicts\n";
  Format.fprintf fmt "  %s\n" (String.concat "\n  " (List.map show_relation (remove_reflexive pc)));

  Format.fprintf fmt "order\n";
  Format.fprintf fmt "  %s\n" (String.concat "\n  " (List.map show_relation (transitive_reduction order)));


  let interesting = longest expected_labels forbidden_labels in
  let exec = List.nth interesting 0 in
  let exec = List.map strip_label exec in

  (* Sorting this makes it easier to think about *)
  let e = build_exec exec order in
  let e = List.sort (fun (E a) (E b) -> compare a b) e in

  Format.fprintf fmt "execution\n";
  Format.fprintf fmt "  %s" (String.concat " " (List.map show_event e));

  Format.fprintf fmt "\n";
  ()
