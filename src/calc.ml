type skel_ret =
  | S_Error of string
  | S_Succ of Constraint.t * Deriv.t
[@@deriving to_yojson]

type infer_ret =
  | I_Error of string
  | I_Succ of Type.aty * Subst.t * Deriv.t
[@@deriving to_yojson]

let _ =
  Js.export_all
    (object%js
      method skel (s : Js.js_string Js.t) : Js.js_string Js.t =
        Type.reset_counter ();
        let lexbuf = Lexing.from_string (Js.to_string s) in
        let ret = try
            let _, e = Parser.top_phrase Lexer.token lexbuf in
            let _, _, cstr, skel = Inference.generate e in
            S_Succ (cstr, skel)
        with
        | _ ->
            S_Error "parse error"
        in
        skel_ret_to_yojson ret |> Yojson.Safe.to_string ~std:true |> Js.string

      method infer (s : Js.js_string Js.t) : Js.js_string Js.t =
        Type.reset_counter ();
        let lexbuf = Lexing.from_string (Js.to_string s) in
        let ret = try
            let _, e = Parser.top_phrase Lexer.token lexbuf in
            let aty, _, cstr, skel = Inference.generate e in
            let st = Inference.unify cstr in
            let deriv = Deriv.applySubst skel st in
            I_Succ (Subst.apply_aty st aty, st, deriv)
          with
          | _ ->
              I_Error "fatal error"
        in
        infer_ret_to_yojson ret |> Yojson.Safe.to_string ~std:true |> Js.string
    end)
