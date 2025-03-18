type primitive = Int of int64 | Float of float | Boolean of bool | String of string | Char of char | Unknown | Unit | Function of string
[@@deriving show, eq, ord]