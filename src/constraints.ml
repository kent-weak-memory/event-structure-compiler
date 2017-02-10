open EventStructure

type constraint_op =
  Eq
| NEq
[@@deriving show]

type constraint_ast =
  Reg of (int * int)
| Bop of (constraint_ast * constraint_op * constraint_ast)
[@@deriving show]

let show_op o =
  match o with
  | Eq -> "=="
  | NEq -> "!="

let rec pp_constraint fmt c =
  match c with
  | Reg (l, v) -> Format.fprintf fmt "R(%d)%d" l v
  | Bop (l, o, r) -> Format.fprintf fmt "%a %s %a"
    pp_constraint l
    (show_constraint_op o)
    pp_constraint r

let rec extract_constraints ev_s =
  match ev_s with
  | Init | Read _ | Write _ | Done ->
    ev_s, []

  | Prod (l, r) ->
    let l, cl = extract_constraints l in
    let r, cr = extract_constraints r in
    Prod (l, r), (cl @ cr)

  | Sum (l, r) ->
    let l, cl = extract_constraints l in
    let r, cr = extract_constraints r in
    Sum (l, r), (cl @ cr)

  | Comp (l, r)->
    let l, cl = extract_constraints l in
    let r, cr = extract_constraints r in
    Comp (l, r), (cl @ cr)

  | Const (l, exit_s) ->
    let l, cl = extract_constraints l in
    l, (exit_s @ cl)

let rec compile_constraints consts reg_map = ()
