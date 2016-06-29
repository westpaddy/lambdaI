let _ =
  Js.export_all
    (object%js
      method skel (s : Js.js_string Js.t) : Js.js_string Js.t =
        let lexbuf = Lexing.from_string (Js.to_string s) in
        let _, e = Parser.top_phrase Lexer.token lexbuf in
        let _, _, _, skel = Inference.generate e in
        Deriv.to_yojson skel |> Yojson.Safe.to_string ~std:true |> Js.string
    end)
