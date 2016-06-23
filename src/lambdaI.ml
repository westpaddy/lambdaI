open Format
open Lang

let () =
  Arg.parse []
    (fun filename -> ())
    "./schale";
  let rec loop env =
    printf "> @?";
    let lexbuf = Lexing.from_channel stdin in
    let new_env = try
        let _, e = Parser.top_phrase Lexer.token lexbuf in
        let _, _, _, skel = Inference.generate e in
        printf "%a@\n" Skel.pp skel; env
      with
      | Failure "lexing: empty token" ->
          printf "Lexing error@\n"; env
      | Parser.Error ->
          printf "Parsing error@\n"; env
    in
    loop new_env
  in
  try
    loop Env.empty
  with
    Lexer.EOF -> printf "@\nbye.@\n"
