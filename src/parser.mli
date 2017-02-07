module T = Tokeniser
type memory_fence = MFence

type id =
  | Source of string
  | Register of string * int
  | Memory of string * int
  [@@deriving show]

type exp =
  | Ident of id
  | Num of int
  | Op of exp * T.op * exp
  | Uop of T.uop * exp
  [@@deriving show]

val pp_exp : Format.formatter -> exp -> unit
val show_exp : exp -> string

type exit_state =
  | Allowed of exp
  | Forbidden of exp

type stmt =
  | Assign of id * exp
  | Ite of exp * stmt * stmt
  | Stmts of stmt list
  | Loc of stmt * int (* for line no annotation *)
  | Par of stmt list list
  | ExitState of exit_state
  | Done
  [@@deriving show]

val show_stmt : stmt -> bytes
val pp_stmt : Format.formatter -> stmt -> unit
val pp_exit_state: Format.formatter -> exit_state -> unit

val parse_program : T.tok_loc list -> stmt list
