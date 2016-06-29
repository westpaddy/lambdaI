open Format

type replacer =
  | T of Type.tvar * Type.aty
  | E of Type.evar * Type.ex

type t = replacer list

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

let apply (st : t) (ty : Type.t) : Type.t =
  List.fold_left
    (fun ty r ->
       match r with
       | T (a, aty) -> Type.replace_tvar ty a aty
       | E (f, ex) -> Type.replace_evar ty f ex)
    ty
    st
