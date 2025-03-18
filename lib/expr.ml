type literal = 
  | LiteralInt of int64
  | LiteralFloat of float
  | LiteralBool of bool
  | LiteralString of string
  | LiteralUnit
  [@@deriving show, eq, ord]

and primitiveType = Unit | Int | Float | Boolean | String | Char
[@@deriving show, eq, ord]

and primary = Literal of literal | PriGroup of expr
[@@deriving show, eq, ord]

and unary = Not of unary | Neg of unary | Primary of primary | Call of call
[@@deriving show, eq, ord]

and factor = FacSingle of unary | Mult of unary * unary | Div of unary * unary
[@@deriving show, eq, ord]

and term = Add of factor * factor | Sub of factor * factor | TerSingle of factor
[@@deriving show, eq, ord]

and comparison = Less of term * term | LessEq of term * term | Greater of term * term | GreaterEq of term * term | ComSingle of term
[@@deriving show, eq, ord]

and equality = Eq of comparison * comparison | NotEq of comparison * comparison | EquSingle of comparison
[@@deriving show, eq, ord]

and arguments = Args of expr list
[@@deriving show, eq, ord]

and call = Func of string * arguments
[@@deriving show, eq, ord]

and expr = Expression of equality | Error of string
[@@deriving show, eq, ord]

