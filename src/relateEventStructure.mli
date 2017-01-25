open Relation
open EventStructure

(* Order, Conflict, val, loc, zero *)
type ev_r = (ev_s relation * ev_s relation) * (ev_s relation * ev_s relation * ev_s relation)

val read_es : ev_s -> ev_s list -> 'a list * (ev_s * ev_s) list -> ev_s list * ('a list * (ev_s * ev_s) list)
