open EventStructure
open Parser

exception ConstraintException of string

type const = (location * value)
  [@@deriving show]

type const_exp =
| Expected of const
| Unexpected of const

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

let rec translate_expression e reg_map =
  match e with
  | Op (Ident (Register (_, ri)), T.Eq, Num v) ->
    let loc = find_in ri reg_map in
    (EventStructure.Loc loc, Val v)
  | Op (Ident (_), _, _) ->
    raise (ConstraintException "Constraints must only be on register values")
  | _ ->
    raise (ConstraintException "Malformed constraint")

let rec compile_constraints consts reg_map =
  match consts with
  | Allowed e :: xs ->
    Expected (translate_expression e reg_map) :: compile_constraints xs reg_map
  | Forbidden e :: xs ->
    Unexpected (translate_expression e reg_map) :: compile_constraints xs reg_map
  | [] -> []
