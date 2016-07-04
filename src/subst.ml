open Format

type replacer =
  | T of Type.tvar * Type.aty
  | E of Type.evar * Type.ex
[@@deriving to_yojson]

type t = replacer list [@@deriving to_yojson]

let pp_replacer (fmt : formatter) (r : replacer) : unit =
  match r with
  | T (a, aty) -> fprintf fmt "%a := @[%a@]" Type.pp_tvar a Type.pp_aty aty
  | E (f, ex) -> fprintf fmt "%a := @[%a@]" Type.pp_evar f Type.pp_ex ex

let pp (fmt : formatter) (st : t) : unit =
  fprintf fmt "{|@[<v>%a@]|}" (pp_print_list pp_replacer) st

let empty = []

let single_t (a : Type.tvar) (aty : Type.aty) : t =
  [T (a, aty)]

let single_e (f : Type.evar) (ex : Type.ex) : t =
  [E (f, ex)]

let compose (st1 : t) (st2 : t) : t =
  st1 @ st2

let apply (st : t) (rt : 'a -> Type.tvar -> Type.aty -> 'a)
    (re : 'a -> Type.evar -> Type.ex -> 'a) (v : 'a) : 'a =
  List.fold_left
    (fun v r ->
       match r with
       | T (a, aty) -> rt v a aty
       | E (f, ex) -> re v f ex)
    v
    st

let apply_aty (st : t) (aty : Type.aty) : Type.aty =
  apply st Type.replace_tvar_aty Type.replace_evar_aty aty

let apply_ty (st : t) (ty : Type.t) : Type.t =
  apply st Type.replace_tvar Type.replace_evar ty
