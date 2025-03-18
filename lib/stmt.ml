type program = Program of statement list | Error of string
[@@deriving show, eq, ord]

and statement = Expr of exprStmt | PrintStmt of printStmt | FuncStmt of func | AssignStmt of assignment
[@@deriving show, eq, ord]

and assignment = Let of string * Impl.primitive
[@@deriving show, eq, ord]

and exprStmt = Expression of Expr.expr 
[@@deriving show, eq, ord]

and printStmt = Print of Expr.expr * printOpts
[@@deriving show, eq, ord]

and printOpts = {
  newline: bool;
  }
[@@deriving show, eq, ord]

and annotation = Main | Init | Fini | EmptyAnnot | Custom of string 
[@@deriving show, eq, ord]

and func = Function of funcDecl * statement
[@@deriving show, eq, ord]

and funcDecl = {
  name: string;
  args: (Expr.primitiveType * string) list;
  return: Expr.primitiveType;
  annotation: annotation;
}
[@@deriving show, eq, ord]