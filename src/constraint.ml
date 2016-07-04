open Format

type t = (Type.t * Type.t) list [@@deriving to_yojson]

let pp (fmt : formatter) (cstr : t) : unit =
  fprintf fmt "{@[<v>%a@]}"
    (pp_print_list
       (fun fmt (ty1, ty2) -> fprintf fmt "@[%a = %a@]" Type.pp ty1 Type.pp ty2))
    cstr

let empty = []

let singleton (ty1 : Type.t) (ty2 : Type.t) : t =
  [ty1, ty2]

let add (cstr : t) (ty1 : Type.t) (ty2 : Type.t) : t =
  if List.mem (ty1, ty2) cstr then cstr else (ty1, ty2) :: cstr

let union (cstr1 : t) (cstr2 : t) : t =
  List.fold_left (fun cstr (ty1, ty2) -> add cstr ty1 ty2) cstr1 cstr2

let fold (cstr : t) (init : 'a) (f : Type.t -> Type.t -> 'a -> t) : t =
  List.fold_left (fun acc (ty1, ty2) -> f ty1 ty2 acc) init cstr

let map (cstr : t) (f : Type.t -> Type.t) : t =
  List.map (fun (ty1, ty2) -> (f ty1, f ty2)) cstr

let applyF (cstr : t) (f : Type.evar) : t =
  map cstr (fun ty -> Type.Expand (f, ty))

let replace_tvar (cstr : t) (a : Type.tvar) (aty : Type.aty) : t =
  map cstr (fun ty -> Type.replace_tvar ty a aty)

let replace_evar (cstr : t) (f : Type.evar) (ex : Type.ex) : t =
  map cstr (fun ty -> Type.replace_evar ty f ex)

let applySubst (cstr : t) (st : Subst.t) : t =
  Subst.apply st replace_tvar replace_evar cstr
