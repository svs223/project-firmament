open In_channel

module L = Func.Lex
module P = Func.Parse
module I = Func.Interpret

let () =
  let f = Sys.argv.(1) in
  let ic = open_text f in
  let txt = input_all ic in
  let lexctx = L.tokenize txt in 
  (* List.iter (fun x -> Printf.printf "%s\n" @@ L.show_token x.L.token_type) lexctx.L.tokens; *)
  let ast = P.parse lexctx.Func.Lex.tokens in
  I.interpret ast
  