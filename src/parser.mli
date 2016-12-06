module T = Tokeniser
type memory_fence = MFence

type id =
  | Source of string
  | Register of string * int
  | Memory of string * int
  [@@deriving ord]

type exp =
  | Ident of id
  | Num of int64
  | Op of exp * T.op * exp
  | Uop of T.uop * exp

type stmt =
  | Assign of id * exp
  | Ite of exp * stmt * stmt
  | Stmts of stmt list
  | Loc of stmt * int (* for line no annotation *)
  | Par of stmt list list
  | Done

val parse_program : T.tok_loc list -> stmt list
