open Format

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
        let () = printf "Skel:@[%a@]@\n" Deriv.pp skel in
        let () = printf "Cstr:@[%a@]@\n" Constraint.pp cstr in
        let st = Inference.unify cstr in
        let () = printf "Unifier:@[%a@]@\n" Subst.pp st in
        let () = printf "Deriv:@[%a@]@\n" Deriv.pp (Deriv.applySubst skel st) in
        let () = printf "Type:@[%a@]@\n" Type.pp_aty (Subst.apply_aty st aty) in
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
