type mtype =
  | I8 of char
  | I64 of int64
  | F64 of float
  [@@deriving show, eq, ord]