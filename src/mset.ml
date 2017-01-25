let mem xs a =
  List.mem a xs

let not_mem xs a = not (mem xs a)

let make_proper s =
  let rec make_proper' acc set =
    match set with
    | x::xs when mem acc x -> make_proper' acc xs
    | x::xs -> make_proper' (x::acc) xs
    | [] -> acc
  in
  make_proper' [] s

let union a b =
  make_proper (a @ b)

let intersection a b =
  List.filter (mem a) b

let subset a b =
  List.for_all (mem b) a

let disjoint a b =
  List.for_all (not_mem b) a

let cross l1 l2 =
   List.fold_left
    (fun x a ->
      List.fold_left (fun y b -> (a, b)::y) x l2
    ) [] l1
