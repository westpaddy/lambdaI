open Format

let json_print_judge (fmt : formatter) (env, e, aty : Deriv.judge) : unit =
  fprintf fmt {|["%a", "%a", "%a"]|} Env.pp env Term.pp e Type.pp_aty aty

let json_print_judge_e (fmt : formatter) (env, e, ty : Deriv.judge_e) : unit =
  fprintf fmt {|["%a", "%a", "%a"]|} Env.pp env Term.pp e Type.pp ty

let rec json_print_deriv (fmt : formatter) (deriv : Deriv.t) : unit =
  match deriv with
  | Deriv.Var j ->
      fprintf fmt
        {|{"rule": "Var", "judge": %a, "premise": []}|}
        json_print_judge j
  | Deriv.Inter (je, deriv1, deriv2) ->
      fprintf fmt
        {|{"rule": "Inter", "judge": %a, "premise": [%a, %a]}|}
        json_print_judge_e je
        json_print_deriv deriv1 json_print_deriv deriv2
  | Deriv.Abs_I (j, deriv) ->
      fprintf fmt
        {|{"rule": "Abs_I", "judge": %a, "premise": [%a]}|}
        json_print_judge j
        json_print_deriv deriv
  | Deriv.Abs_K (j, deriv) ->
      fprintf fmt
        {|{"rule": "Abs_K", "judge": %a, "premise": [%a]}|}
        json_print_judge j
        json_print_deriv deriv
  | Deriv.App (j, deriv1, deriv2) ->
      fprintf fmt
        {|{"rule": "App", "judge": %a, "premise": [%a, %a]}|}
        json_print_judge j
        json_print_deriv deriv1 json_print_deriv deriv2
  | Deriv.F (je, deriv) ->
      fprintf fmt
        {|{"rule": "F", "judge": %a, "premise": [%a]}|}
        json_print_judge_e je
        json_print_deriv deriv

let _ =
  Js.export_all
    (object%js
      method skel (s : Js.js_string Js.t) : Js.js_string Js.t =
        let lexbuf = Lexing.from_string (Js.to_string s) in
        let _, e = Parser.top_phrase Lexer.token lexbuf in
        let _, _, _, skel = Inference.generate e in
        asprintf "%a" json_print_deriv skel |> Js.string
    end)
