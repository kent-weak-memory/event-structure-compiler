open EventStructure

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
