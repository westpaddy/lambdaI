open Format

type var = string             (* lambda variable: x *)
[@@deriving to_yojson]

type t =
  | Var of var                (* lambda variable: x *)
  | App of t * t              (* application: e1 e2 *)
  | Abs of var * t            (* abstraction: \x.e *)
[@@deriving to_yojson]

let rec pp (fmt : formatter) (e : t) : unit =
  match e with
  | Var x ->
      fprintf fmt "%s" x
  | App (e1, e2) ->
      fprintf fmt "@[(%a@ %a)@]" pp e1 pp e2
  | Abs (x, e) ->
      fprintf fmt "@[(fun %s ->@ %a)@]" x pp e
