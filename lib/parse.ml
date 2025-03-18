type parser_ctx = {
  current: int;
  tokens: Lex.token_info list;
  table: Impl.primitive Table.SymMap.t;
}

(* and table_value = Function of Stmt.funcDecl *)

type acc_lin = {
  name: string;
  annot: Stmt.annotation;
  return: Expr.primitiveType;
  current: acc_type;
  arguments: (Expr.primitiveType * string) list;
}

and acc_type = Name | Annot | Return | Undefined | Args

type scope = Global 

and var_type = {
  ident: string;
  value: Impl.primitive;
  scope: scope;
}

exception ParseException of string * parser_ctx option

(* let find_func name ctx = List.find (fun v -> match v with Function f -> f.name = name) ctx.table *)

let parsefail ?(msg="syntax error") ?ctx () = raise (ParseException (msg, ctx))
let check expected ctx = try expected = (List.nth ctx.tokens @@ ctx.current - 1).token_type with Invalid_argument _ -> parsefail ~msg:"line 11" () 
let peek ctx = try Some (List.nth ctx.tokens ctx.current).token_type with Invalid_argument _ -> None
let dpeek ctx = try Some (List.nth ctx.tokens @@ ctx.current + 1).token_type with Invalid_argument _ -> None
let current ctx = 
  try Some(List.nth ctx.tokens @@ ctx.current - 1).token_type
  with Invalid_argument msg -> Printf.printf "Exception: %s\nIndex: %d\n" msg ctx.current; None
let current_literal ctx = try (List.nth ctx.tokens @@ ctx.current - 1).value with Invalid_argument _ -> None
let prev_literal ctx = try (List.nth ctx.tokens @@ ctx.current - 2).value with Invalid_argument _ -> None
let is_at_end ctx = match peek ctx with 
| Some Lex.EOF | None -> true
| _ -> false

let current_lexeme ctx = try Some ((List.nth ctx.tokens @@ ctx.current - 1).lexeme) with Invalid_argument _ -> None
let peek_lexeme ctx = try Some ((List.nth ctx.tokens @@ ctx.current).lexeme) with Invalid_argument _ -> None

let advance ctx = 
  (* Printf.printf "advancing from %d to %d\n" ctx.current @@ ctx.current + 1; *)
  if is_at_end ctx then ctx else {ctx with current = ctx.current + 1 }

let advance_doub ctx = advance @@ advance ctx
let advance_tri ctx = advance @@ advance @@ advance ctx

let match_set set ctx = 
  let token = match current ctx with Some t -> t | None -> parsefail ~msg:"unexpected end of input" ~ctx () in
  let rec checker acc obj = match (obj, acc) with 
  | (t, _) when t = token -> true
  | (_, []) -> false
  | (_, lst) -> checker (List.tl lst) (List.hd lst)
  in checker (List.tl set) (List.hd set)

let match_token expected ctx = match current ctx with 
  | Some t when t = expected -> true
  | _ -> false
let consume token ctx = 
  if check token ctx then advance ctx
  else parsefail ~msg:(Printf.sprintf "unexpected input: %s" @@ Lex.show_token @@ Option.get @@ current ctx) ()

let rec parse_expression ctx = 
  let equ, adv = parse_equality ctx in
  (Expr.Expression equ), adv

and parse_call ctx =
  let r, adv = parse_primary ctx in
  let right = match r with 
  | Expr.Literal (Expr.LiteralString s) -> s
  | _ -> parsefail ~msg:"Function call is of wrong type" ()
  in
  let rec loop ctx acc = 
    match current ctx with 
    | Some Lex.Semicolon -> acc
    | Some _ -> 
      let ex, adv = parse_expression ctx in
      loop adv @@ acc @ [ex]
    | None -> parsefail ~msg:"unexpected end of input" ()
  in 
  let args = Expr.Args (loop adv []) in
  Expr.Func (right, args)

and parse_primary ctx = 
  let print_current ctx = match current ctx with 
  | Some t -> Lex.show_token t
  | None -> "<none>"
  in
  (* Printf.printf "Primary Level: %s\n" @@ Lex.show_token @@ Option.get @@ current ctx; *)
  if match_token Lex.False ctx
  then Expr.Literal (Expr.LiteralBool (false)), ctx
  else if match_token Lex.True ctx
  then Expr.Literal (Expr.LiteralBool (true)), ctx
  else if match_token Lex.Literal ctx 
  then match current_literal ctx with 
  | Some (Lex.IntegerVal i) -> Expr.Literal ((Expr.LiteralInt i)), (advance ctx)
  | Some (Lex.FloatVal f) -> Expr.Literal ((Expr.LiteralFloat f)), (advance ctx)
  | Some f -> parsefail ~msg:(Printf.sprintf "Unsupported feature: %s\n" @@ Lex.show_literal f) ()
  | None -> parsefail ~msg:"line 70" ()
  else if match_token Lex.StringLiteral ctx
  then begin 
    let lit = match current_literal ctx with Some t -> t | None -> parsefail ~msg:"expected literal" () in
    match lit with 
    | StringVal s -> Expr.Literal ((Expr.LiteralString s)), (advance ctx)
    | _ -> parsefail ~msg:"string expected" ()
  end else if match_token Lex.LPara ctx
  then begin
    let expr, adv = parse_expression @@ advance ctx in
    let final = consume RPara adv in
    (Expr.PriGroup expr), final
  end else if match_token Lex.Identifier ctx 
  then begin 
    let ctxval = match current_lexeme ctx with Some x -> x | None -> parsefail ~msg:"unknown error" ()in
    let valu = Table.SymMap.find_first_opt (fun x -> x = ctxval) ctx.table in
    match valu with 
    | Some v -> 
      let typ = (match v with 
      | _, Impl.Int i -> (Expr.Literal (Expr.LiteralInt i))
      | _, Impl.Boolean b -> (Expr.Literal (Expr.LiteralBool b))
      | _, Impl.Float f -> (Expr.Literal (Expr.LiteralFloat f))
      | _, Impl.String s -> (Expr.Literal (Expr.LiteralString s))
      | _, Impl.Function _ -> parsefail ~msg:"call error" ()
      | _, Impl.Unit -> (Expr.Literal (Expr.LiteralUnit))
      | _, _ -> parsefail ~msg:"unknown type" ())
      in
      typ, advance ctx
    | None -> parsefail ~msg:(Printf.sprintf "unknown symbol: %s" ctxval) ()
  end else let _ = Printf.printf "->%s:%d<-\n" (print_current ctx) (ctx.current) in parsefail ~msg:"code 90" ()

and parse_unary ctx = 
  (* let tok = match current ctx with Some t -> Lex.show_token t | None -> "<none>" in *)
  (* Printf.printf "Unary Level: %s\n" tok; *)
  if match_set [Lex.Bang; Lex.Minus] ctx
  then begin
    let op = match current ctx with Some t -> t | None -> parsefail ~msg:"code 80" () in
    let right, adv1 = parse_unary @@ advance ctx in
    let prod = match op with 
    | Lex.Bang -> Expr.Not (right)
    | Lex.Minus -> Expr.Neg (right)
    | _ -> parsefail ~msg:"code 85" ()
    in
    prod, advance adv1
  end else begin
    (* let adv0 = advance ctx in *)
    let pri, adv = parse_primary ctx in
    Expr.Primary (pri), adv
  end
and parse_factor ctx = 
  let expr, advanced_ctx = parse_unary ctx in
  (* Printf.printf "Factor Level: %s\n" @@ Expr.show_unary expr; *)
  let loop ctx = 
    if (match_set [Lex.Star; Lex.Slash] ctx)
    then begin
      let op = match current ctx with Some t -> t | None -> parsefail ~msg:"code 98" () in
      let adv0 = advance ctx in
      let right, adv1 = parse_unary adv0 in
      let prod = match op with 
      | Lex.Star -> Expr.Mult (expr, right)
      | Lex.Slash -> Expr.Div (expr, right)
      | _ -> parsefail ~msg:"code 103" () 
      in
      prod, adv1
    end else Expr.FacSingle (expr), advanced_ctx
  in loop advanced_ctx

and parse_term ctx = 
  let expr, advanced_ctx = parse_factor ctx in
  (* Printf.printf "Term Level: %s\n" @@ Expr.show_factor expr; *)
  let loop ctx = 
    if (match_set [Lex.Minus; Lex.Plus] ctx)
    then begin
      let op = match current ctx with Some t -> t | None -> parsefail ~msg:"code 116" () in
      let adv0 = advance ctx in
      let right, adv1 = parse_factor adv0 in
      let prod = match op with 
      | Lex.Minus -> Expr.Sub (expr, right)
      | Lex.Plus -> Expr.Add (expr, right)
      | _ -> parsefail ~msg:"code 121" ()
      in
      (* let adv = advance adv1 in *)
      prod, adv1
    end else Expr.TerSingle (expr), ctx
  in loop advanced_ctx

and parse_comparison ctx = 
  let expr, advanced_ctx = parse_term ctx in
  (* Printf.printf "Comparison Level: %s\n" @@ Expr.show_term expr; *)
  let loop ctx = 
    let _curop = match current ctx with Some op -> Lex.show_token op | None -> parsefail ~msg:"code 134" () in
    let _curlit = match current_literal ctx with Some lit -> Lex.show_literal lit | None -> "<none>" in
    (* Printf.printf "%s:%s\n" curop curlit; *)
    if (match_set [Lex.Greater; Lex.GreaterEq; Lex.Less; Lex.LessEq] ctx)
    then begin
      let op = match current ctx with Some t -> t | None -> parsefail ~msg:"code 139" () in
      let adv0 = advance ctx in
      let right, adv1 = parse_term adv0 in
      let prod = match op with 
      | Lex.Greater -> Expr.Greater (expr, right)
      | Lex.GreaterEq -> Expr.GreaterEq (expr, right)
      | Lex.Less -> Expr.Less (expr, right)
      | Lex.LessEq -> Expr.LessEq (expr, right)
      | _ -> parsefail ~msg:"code 147" () 
      in
      (* let adv = advance adv1 in *)
      prod, adv1
    end else Expr.ComSingle (expr), ctx
  in loop advanced_ctx

and parse_equality xctx =
  let expr, advanced_ctx = parse_comparison xctx in
  (* Printf.printf "Equality Level: %s\n" @@ Expr.show_comparison expr; *)
  let loop ctx = 
    let _curop = match current ctx with Some op -> Lex.show_token op | None -> parsefail ~msg:"code 158" () in
    let _curlit = match current_literal ctx with Some lit -> Lex.show_literal lit | None -> "<none>" in
    (* Printf.printf "%s:%s\n" curop curlit; *)
    if (match_set [Lex.Eq; Lex.NotEq] ctx)
    then begin
      let op = match current ctx with Some t -> t | None -> parsefail ~msg:"code 163" () in
      let right, adv1 = parse_comparison @@ advance ctx in
      let prod = match op with 
      | Lex.Eq -> Expr.Eq (expr, right)
      | Lex.NotEq -> Expr.NotEq (expr, right)
      | _ -> parsefail ~msg:"code 168" ()
      in
      (* let adv = advance adv1 in *)
      prod, adv1
    end else Expr.EquSingle (expr), ctx
  in loop advanced_ctx

and parse_exprstmt ctx =
  let expr, adv0 = parse_expression ctx in
  let adv = consume Lex.Semicolon adv0 in
  (Stmt.Expr (Stmt.Expression expr)), adv

and parse_print ctx =
  let value, adv0 = parse_expression ctx in
  let adv = consume Lex.Semicolon adv0 in
  (Stmt.PrintStmt (Stmt.Print (value, {newline = false;}))), adv

and parse_println ctx =
  let value, adv0 = parse_expression ctx in
  let adv = consume Lex.Semicolon adv0 in
  (Stmt.PrintStmt (Stmt.Print (value, {newline = true;}))), adv

and parse_annot name ctx =
  let annot = match name with 
  | Some "@main" -> Stmt.Main
  | Some "@init" -> Stmt.Init
  | Some "@fini" -> Stmt.Fini
  | Some s -> Stmt.Custom (s)
  | None -> Stmt.EmptyAnnot
  in
  parse_fun annot @@ advance ctx
  
and parse_type str = match str with 
| "unit" -> Expr.Unit
| "int" -> Expr.Int
| "float" -> Expr.Float
| "bool" -> Expr.Boolean
| "char" -> Expr.Char
| "string" -> Expr.String
| _ -> parsefail ~msg:(Printf.sprintf "unknown type: %s" str) ()

and parse_fun_loop acc ctx = match (current ctx, peek ctx, dpeek ctx) with
| (Some Lex.Identifier, Some Lex.Colon, _) -> parse_fun_loop {acc with name = (Option.get @@ current_lexeme ctx)} @@ advance_doub ctx
| (Some Lex.Identifier, Some Lex.Identifier, Some Lex.Arrow) -> parse_fun_loop {acc with arguments = acc.arguments @ [(parse_type @@ Option.get @@ peek_lexeme ctx, Option.get @@ current_lexeme ctx)]} @@ advance_tri ctx
| (Some Lex.Identifier, Some Lex.Equals, _) -> {Stmt.name = acc.name; Stmt.args = acc.arguments; Stmt.return = (parse_type @@ Option.get @@ current_lexeme ctx); Stmt.annotation = EmptyAnnot}, advance_doub ctx
| (Some Lex.Arrow, _, _) -> parse_fun_loop acc @@ advance ctx
| x, y, z -> parsefail ~msg:(
  Printf.sprintf "unexpected tokens (%s, %s, %s)" 
  (Lex.show_token_option x)
  (Lex.show_token_option y)
  (Lex.show_token_option z)
  ) ()
and parse_fun_decl acc ctx = 
  let fn, adv = parse_fun_loop acc ctx in
  Printf.printf "%s\n" @@ Stmt.show_funcDecl fn;
  fn, adv

and parse_fun annot ctx = 
  let decl, adv0 = parse_fun_decl {name = ""; annot = annot; return = Expr.Unit; current = Undefined; arguments = []; } ctx in
  let stmt, adv1 = parse_stmt adv0 in
  Stmt.Function (decl, stmt), adv1

and parse_primitive = function
| Some Lex.IntegerVal i -> Impl.Int (i)
| Some Lex.FloatVal f -> Impl.Float (f)
| Some Lex.StringVal s -> Impl.String(s)
| _ -> parsefail ~msg:"Unknown: code 278" ()

and parse_let ctx = 
  let rec parse_let' acc ctx =
    let unwrap x = match x with Some y -> y | None -> "<none>" in
    match current ctx with 
    | Some Lex.Identifier -> parse_let' {acc with ident = (unwrap @@ current_lexeme ctx)} @@ advance ctx
    | Some Lex.Semicolon -> acc, advance ctx
    | Some Lex.Equals -> parse_let' acc @@ advance ctx
    | Some Lex.Literal | Some Lex.StringLiteral -> parse_let' {acc with value = (parse_primitive @@ current_literal ctx)} @@ advance ctx
    | Some Lex.Let -> parse_let' acc @@ advance ctx
    | Some other -> parsefail ~msg:(Printf.sprintf "Invalid Lexeme: %s/%s" (Lex.show_token other) (unwrap @@ current_lexeme ctx)) ()
    | None -> parsefail ~msg:"Unexpected end of file" ()
  in
  let vl, adv = parse_let' {ident = ""; scope = Global; value = Impl.Unit} ctx in
  Stmt.Let (vl.ident, vl.value), {adv with table = Table.SymMap.add vl.ident vl.value adv.table}
  
and parse_stmt ctx = 
  match current ctx with 
  | Some Lex.Print -> parse_print @@ advance ctx
  | Some Lex.Println -> parse_println @@ advance ctx
  | Some Lex.Annotation -> 
    let stmt, adv = parse_annot (current_lexeme ctx) (advance ctx) in
    Stmt.FuncStmt(stmt), adv
  | Some Lex.Fun -> 
    let stmt, adv = parse_fun EmptyAnnot @@ advance ctx in
    Stmt.FuncStmt(stmt), adv
  | Some Lex.Let ->
    let stmt, adv = parse_let @@ advance ctx in
    Stmt.AssignStmt (stmt), adv
  | Some _ -> parse_exprstmt @@ advance ctx
  | None -> parsefail ~msg:"unexpected end of input" ()

and parse_loop ctx acc =
  if is_at_end ctx then acc
  else begin 
    let item, adv = parse_stmt ctx in
    parse_loop adv @@ acc @ [item]
  end

and parse tokens =
  let ctx = {
    tokens;
    current = 1;
    table = Table.SymMap.empty;
  } in Stmt.Program (parse_loop ctx [])