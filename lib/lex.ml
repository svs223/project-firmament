type token = 
  | Identifier
  | StringLiteral
  | Literal
  | EOF
  | LPara
  | RPara
  | LBrac
  | RBrac
  | Colon
  | Semicolon
  | Star
  | Dot
  | Let
  | Plus
  | Minus
  | Slash
  | False
  | True
  | Unit
  | Equals
  | NotEq
  | Eq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | Bang
  | Fun
  | Rec
  | If
  | Then
  | Else
  | Annotation
  | Arrow
  | Print
  | Println
  | Unique
  [@@deriving show, ord, eq]

type annotation = Main | Init | Fini | Custom of string
  [@@deriving show, ord, eq]

type literal =
  | StringVal of string
  | IntegerVal of int64
  | FloatVal of float
  | IdentifierVal of string
  | AnnotationVal of annotation
  [@@deriving show, ord, eq]

type token_info = {
  token_type: token;
  lexeme: string;
  value: literal option;
  line: int;
}
[@@deriving show, ord, eq]

type lexer_ctx = {
  source: string;
  start: int;
  current: int;
  line: int;
  tokens: token_info list;
}
[@@deriving show, ord, eq]

let show_token_option t = match t with 
| Some t -> show_token t
| None -> "<none>"

let lexfail ?(msg="syntax error") ?target ctx = 
  match target with 
  | Some char -> failwith @@ Printf.sprintf "%s: %s on line %d" msg char ctx.line
  | None -> failwith @@ Printf.sprintf "%s on line %d" msg ctx.line

let show_literal_option lit = match lit with
  | Some x -> show_literal x
  | None -> "<none>"

let is_at_end ctx = ctx.current >= (String.length ctx.source)

let is_digit char = match char with
  | Some c when c >= '0' && c <= '9' -> true
  | _ -> false

let is_whitespace char = match char with
  | Some '\n' | Some '\t' | Some ' ' | Some '\r' -> true
  | _ -> false

let is_alpha char = match char with
  | Some c when c >= 'a' && c <= 'z' -> true
  | Some c when c >= 'A' && c <= 'Z' -> true
  | Some '_' -> true
  | _ -> false

let is_alnum char = is_alpha char || is_digit char
let advance ctx = {ctx with current = ctx.current + 1}
let current_char ctx = 
  try 
    Some (String.get ctx.source (ctx.current - 1))
  with Invalid_argument _ -> None

let next_char ctx = 
  try 
    Some (String.get ctx.source (ctx.current))
  with Invalid_argument _ -> None

let match_char expected ctx =
  if (is_at_end ctx) then (false, ctx)
  else match next_char ctx with
  | Some c when c = expected -> (true, advance ctx)
  | _ -> (false, ctx)

let add_token token_type lexeme value line ctx =
  {ctx with tokens = ctx.tokens @ [{
    token_type;
    lexeme;
    value;
    line
    }]
  }
let add_literal_token token_type literal ctx =
  let text = try (String.sub ctx.source ctx.start (ctx.current - ctx.start)) with Invalid_argument msg -> failwith @@ Printf.sprintf "%s on line %d\nIndex of (%d - %d = %d)" msg ctx.line ctx.current ctx.start (ctx.current - ctx.start) in
  add_token token_type text literal ctx.line ctx

let add_eof ctx = add_token EOF "" None ctx.line ctx

let increment_line ctx = {ctx with line = ctx.line + 1}

let add_non_literal_token token_type ctx = add_literal_token token_type None ctx

let add_cond_non_literal_token expected tok_if_true tok_if_false ctx = 
  match (match_char expected ctx) with
  | (true, new_ctx) -> add_non_literal_token tok_if_true new_ctx
  | (false, new_ctx) -> add_non_literal_token tok_if_false new_ctx

let add_cond_slash ctx =
  let rec consume_line ctx =
    match (not (next_char ctx = Some '\n')), not (is_at_end ctx) with
    | (true, true) -> consume_line (advance ctx)
    | _ -> ctx
  in
  if (fst (match_char '/' ctx)) then consume_line ctx else add_non_literal_token Slash ctx

let peek ctx = 
  try Some (String.get ctx.source (ctx.current + 1)) 
  with Invalid_argument _ -> None

let capture_digits ctx starter =
  let rec loop acc ctx =
    let advanced_ctx = advance ctx in
    match current_char advanced_ctx with
    | Some c when is_digit (Some c) || c = '.' -> loop (acc @ [c]) advanced_ctx
    | Some '_' -> loop acc advanced_ctx
    | Some c when is_whitespace (Some c) -> (acc, advanced_ctx)
    | _ -> (acc, ctx)
  in 
  let (digits, ctx) = loop [starter] ctx in
  let numstr = String.concat "" @@ List.map (String.make 1) digits in
  (* Printf.printf "---\n%s\n---\n" numstr; *)
  if String.contains numstr '.'
  then (FloatVal (Float.of_string numstr), ctx)
  else (IntegerVal (Int64.of_string numstr), ctx)

let add_string_literal ctx =
  (* let exception Complete of char list in *)
  let escape ctx = 
    let c = current_char ctx in
    match c with 
    | Some 'n' -> '\n'
    | Some 't' -> '\t'
    | Some 'r' -> '\r'
    | Some '\\' -> '\\'
    | Some '\'' -> '\''
    | Some '\"' -> '"'
    | Some _ -> lexfail ~msg:"unknown escape sequence" ctx
    | None -> lexfail ~msg:"unexpected end of string" ctx
  in
  let rec loop acc ctx = 
    let adv = advance ctx in
    match current_char adv with
    | Some '"' -> (acc, ctx)
    | Some '\\' -> loop (acc @ [escape @@ advance adv]) @@ advance adv
    | Some c -> loop (acc @ [c]) adv
    | None -> failwith "unexpected end of input" 
  in 
  let (chars, adv) = loop [] ctx in
  let str = String.concat "" @@ List.map (String.make 1) chars in
  (* Printf.printf "%s\n" str; *)
  add_literal_token StringLiteral (Some (StringVal str)) @@ advance adv

let add_num_literal ctx num =
  let (lit, advanced_ctx) = capture_digits ctx num in
  add_literal_token Literal (Some lit) advanced_ctx 

let rec add_identifier_literal ctx =
  (* Printf.printf "Ident\n"; *)
  if (is_alnum (next_char ctx)) then add_identifier_literal @@ advance ctx 
  else begin
    let substr = (String.sub ctx.source ctx.start (ctx.current - ctx.start)) in
    match substr with
    | "let" -> add_non_literal_token Let ctx
    | "fun" -> add_non_literal_token Fun ctx
    | "true" -> add_non_literal_token True ctx
    | "false" -> add_non_literal_token False ctx
    | "print" -> add_non_literal_token Print ctx
    | "uniq" -> add_non_literal_token Unique ctx
    | "if" -> add_non_literal_token If ctx
    | "then" -> add_non_literal_token Then ctx
    | "else" -> add_non_literal_token Else ctx
    | "println" -> add_non_literal_token Println ctx
    | _ -> add_literal_token Identifier (Some (IdentifierVal substr)) ctx
  end

let windows_newline ctx = match peek ctx with
  | Some '\n' -> advance @@ increment_line ctx
  | _ -> ctx

let rec add_annotation ctx =
  if (is_alpha (next_char ctx)) then add_annotation @@ advance ctx
  else begin
    let substr = (String.sub ctx.source ctx.start (ctx.current - ctx.start)) in
    match substr with 
    | "@main" -> add_literal_token Annotation (Some (AnnotationVal Main)) ctx
    | "@init" -> add_literal_token Annotation (Some (AnnotationVal Init)) ctx
    | "@fini" -> add_literal_token Annotation (Some (AnnotationVal Fini)) ctx 
    | name -> add_literal_token Annotation (Some (AnnotationVal (Custom name))) ctx
  end

let scan_token ctx =
  let advanced_ctx = advance ctx in
  match (current_char advanced_ctx) with 
  | Some '(' -> add_cond_non_literal_token ')' Unit LPara advanced_ctx
  | Some ')' -> add_non_literal_token RPara advanced_ctx
  | Some ';' -> add_non_literal_token Semicolon advanced_ctx
  | Some ':' -> add_non_literal_token Colon advanced_ctx
  | Some '{' -> add_non_literal_token LBrac advanced_ctx
  | Some '}' -> add_non_literal_token RBrac advanced_ctx
  | Some '.' -> add_non_literal_token Dot advanced_ctx
  | Some '+' -> add_non_literal_token Plus advanced_ctx
  | Some '-' -> add_cond_non_literal_token '>' Arrow Minus advanced_ctx
  | Some '*' -> add_non_literal_token Star advanced_ctx
  | Some '/' -> add_cond_slash advanced_ctx
  | Some '!' -> add_cond_non_literal_token '=' NotEq Bang advanced_ctx
  | Some '=' -> add_cond_non_literal_token '=' Eq Equals advanced_ctx
  | Some '<' -> add_cond_non_literal_token '=' LessEq Less advanced_ctx
  | Some '>' -> add_cond_non_literal_token '=' GreaterEq Greater advanced_ctx
  | Some '"' -> add_string_literal advanced_ctx
  | Some '\n' -> increment_line advanced_ctx (* Unix endings *)
  | Some '\r' -> windows_newline ctx (* windows line endings *)
  | Some '@' -> add_annotation advanced_ctx
  | Some c when is_digit (Some c) -> add_num_literal advanced_ctx c
  | Some c when is_alpha (Some c) -> add_identifier_literal advanced_ctx
  | c when is_whitespace c -> advanced_ctx
  | Some c -> lexfail ~msg:"unexpected character" ~target:(String.make 1 c) advanced_ctx
  | None -> advanced_ctx

let tokenize txt = 
  let ctx = {
    source = txt;
    start = 0;
    line = 1;
    current = 0;
    tokens = [];
  } in
  let update ctx = {ctx with start = ctx.current} in
  let rec loop ctx =
    match (peek ctx) with 
    | Some _ -> loop @@ update @@ scan_token ctx
    | None -> add_eof @@ update @@ scan_token ctx
  in loop ctx