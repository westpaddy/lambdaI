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
        let aty, _, cstr, skel = Inference.generate e in
        let () = printf "Skel:@[%a@]@\n" Skel.pp skel in
        let () = printf "Cstr:@[%a@]@\n" Constraint.pp cstr in
        let () = printf "Type:@[%a@]@\n" Type.pp_aty aty in
        let () = printf "SimplCstr:@[%a@]@\n" Constraint.pp (Inference.simplify cstr) in
        env
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
