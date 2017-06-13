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
  let succs = List.fold_left (@) [] (List.map (get_succs order) evs) in
  let preds = List.fold_left (@) [] (List.map (get_preds order) evs) in
  Mset.union succs preds

let longest a b =
  if List.length a > List.length b then a else b

let print_sim fmt long var_map test_name events labels rels pc (expected_labels, forbidden_labels) =
  Format.fprintf fmt "events %d\n" (List.length events);
  let reads = get_reads labels in
  Format.fprintf fmt "reads %s\n" (String.concat " " (List.map (fun f -> show_event (strip_label f)) reads));

  let order, conflict = rels in

  Format.fprintf fmt "justifies\n";
  print_justifies fmt (get_all_justifies reads labels);

  Format.fprintf fmt "conflicts\n";
  Format.fprintf fmt "  %s\n" (String.concat "\n  " (List.map show_relation (remove_reflexive pc)));

  Format.fprintf fmt "order\n";
  Format.fprintf fmt "  %s\n" (String.concat "\n  " (List.map show_relation (transitive_reduction order)));


  let interesting = longest expected_labels forbidden_labels in
  let exec = List.nth interesting 0 in
  let exec = List.map strip_label exec in

  let foo = build_exec exec order in
  let foo = List.sort (fun (E a) (E b) -> compare a b) foo in

  Format.fprintf fmt "execution\n";
  Format.fprintf fmt "  %s" (String.concat " " (List.map show_event foo));

  Format.fprintf fmt "\n";
  ()
