type value = Number of int64 | String of string | Boolean of bool | Call of string | Unit

exception RuntimeException of string

type argument = ArgumentMap of string * value

and arguments = argument list

type var_map = string * value

type env_type = {
  var: var_map list;
}

let symbols : (string, Impl.primitive) Hashtbl.t = Hashtbl.create 64

let runtimefail ?(msg="runtime error") () = raise (RuntimeException msg)

let funcTable : (string, Stmt.statement * Stmt.funcDecl) Hashtbl.t = Hashtbl.create 48 

let get_arg name map = List.find (fun x -> let x, _ = x in x = name) map

let primitive_type_of_value value = match value with 
| Number _ -> Expr.Int
| String _ -> Expr.String
| Boolean _ -> Expr.Boolean
| Unit -> Expr.Unit 
| _ -> runtimefail ~msg:"unimplemented" ()

let rec interpret_print stmt = 
  let expr, opts = match stmt with 
  | Stmt.Print (expr, opts) -> expr, opts
  in
  (* let expr, opts = stmt in *)
  let toprint = interpret_expr expr in
  let str value = match value with
  | Number i -> Int64.to_string i
  | String s -> s 
  | Boolean b -> Bool.to_string b
  | Call _ -> 
    runtimefail ~msg:"unimplemented" ()
    (* str @@ interpret_call fname *)
  | Unit -> "()"
  in
  if opts.Stmt.newline then begin Printf.printf "%s\n" @@ str toprint; Unit 
  end else begin Printf.printf "%s" @@ str toprint; Unit end

and interpret_assignment = function 
| Stmt.Let (s, prim) -> Hashtbl.add symbols s prim; Unit

and interpret_expr expr = match expr with 
  | Expr.Expression equ -> interpret_equ equ
  | Expr.Error _ -> runtimefail ()

and interpret_equ equ = 
  match equ with 
  | Expr.Eq (comp1, comp2) -> 
    let val1 = interpret_com comp1 in
    let val2 = interpret_com comp2 in
    Boolean (val1 = val2) 
  | Expr.NotEq (comp1, comp2) ->
    let val1 = interpret_com comp1 in
    let val2 = interpret_com comp2 in
    Boolean (val1 <> val2)
  | Expr.EquSingle comp -> interpret_com comp

and interpret_com comp = match comp with 
| ComSingle term -> interpret_term term
| _ -> runtimefail ~msg:"unimplemented" ()

and interpret_term term = match term with 
  | Add (f1, f2) -> 
    let unwrap fac = match fac with 
    | Number n -> n
    | _ -> runtimefail ~msg:"unsupported operation" ()
    in
    let fac1 = unwrap @@ interpret_factor f1 in
    let fac2 = unwrap @@ interpret_factor f2 in
    Number (Int64.add fac1 fac2)
  | Sub (f1, f2) ->
    let unwrap fac = match fac with 
    | Number n -> n
    | _ -> runtimefail ~msg:"unsupported operation" ()
    in
    let fac1 = unwrap @@ interpret_factor f1 in
    let fac2 = unwrap @@ interpret_factor f2 in
    Number (Int64.sub fac1 fac2)
  | TerSingle factor -> interpret_factor factor

and interpret_factor factor = match factor with 
  | Expr.FacSingle unary -> interpret_unary unary
  | Expr.Div (u1, u2) -> 
    let unwrap fac = match fac with 
    | Number n -> n
    | _ -> runtimefail ~msg:"unsupported operation" ()
    in
    let una1 = unwrap @@ interpret_unary u1 in
    let una2 = unwrap @@ interpret_unary u2 in
    Number (Int64.div una1 una2)
  | Expr.Mult (u1, u2) ->
    let unwrap fac = match fac with 
    | Number n -> n
    | _ -> runtimefail ~msg:"unsupported operation" ()
    in
    let una1 = unwrap @@ interpret_unary u1 in
    let una2 = unwrap @@ interpret_unary u2 in
    Number (Int64.mul una1 una2)

and interpret_unary unary = 
  match unary with 
  | Expr.Primary pri -> interpret_primary pri
  | Expr.Neg u -> 
    let unwrap unwr = match unwr with
    | Number n -> n
    | _ -> runtimefail ~msg:"unsupported operation" ()
    in
    Number (Int64.mul Int64.minus_one (unwrap (interpret_unary u)))
    
  | Expr.Not u -> 
    let unwrap unwr = match unwr with
    | Boolean b -> b
    | _ -> runtimefail ~msg:"unsupported operation" ()
    in    
    Boolean (not @@ unwrap @@ interpret_unary u)

  | Expr.Call _ -> 
    runtimefail ~msg:"unimplemented" ()
    (* let name, args = match call with 
    | Func (s, a) -> s, a
    in

    interpret_call name  *)

  (* | _ -> runtimefail ~msg:"unimplemented" () *)
  
and interpret_primary primary = match primary with
 | Expr.Literal lit -> rewrap_literal lit
 | Expr.PriGroup grp -> interpret_expr grp

and interpret_call name args = 
  let arguments = match args with 
  | Expr.Args l -> List.map (interpret_expr) l
  in
  let func = Hashtbl.find funcTable name in
  let stmt, decl = func in
  let _zip = try List.combine decl.args arguments with Invalid_argument _ -> runtimefail ~msg:"arguments of wrong length" () in
  
  (* Chcek type and fail if different than defined*)
  (* let zip2 = List.map (fun x -> 
    let (expected, ident), value = x in
    let real = primitive_type_of_value value in
    if expected = real then ident, value
    else (runtimefail ~msg:"Incorrect type" ())
  ) zip in *)

  interpret_stmt stmt

 and interpret_func func = match func with
  Stmt.Function (decl, expr) -> 
    Hashtbl.add funcTable decl.Stmt.name (expr, decl); Unit

 and rewrap_literal lit = match lit with 
  | Expr.LiteralInt i -> Number i
  | Expr.LiteralBool b -> Boolean b
  | Expr.LiteralFloat f -> Number (Int64.of_float f)
  | Expr.LiteralString s -> String s
  | Expr.LiteralUnit -> Unit
and interpret_stmt stmt = match stmt with 
  | Stmt.PrintStmt print -> interpret_print print
  | Stmt.Expr e -> interpret_expr_stmt e
  | Stmt.AssignStmt a -> interpret_assignment a
  | Stmt.FuncStmt _ -> runtimefail ~msg:"unimplemented" ()
  (* | Stmt.FuncStmt f -> interpret_func f *)

and interpret_expr_stmt expr = match expr with 
    | Stmt.Expression e -> interpret_expr e

let interpret program = 
  let ast = match program with 
  | Stmt.Program ast -> ast
  | Stmt.Error msg -> runtimefail ~msg ()
  in
  let _ = List.map interpret_stmt ast in
  ()