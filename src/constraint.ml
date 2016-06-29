open Format

type t = (Type.t * Type.t) list

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

let applyF (cstr : t) (f : Type.evar) : t =
  List.map (fun (ty1, ty2) -> (Type.Expand (f, ty1), Type.Expand (f, ty2))) cstr

let fold (cstr : t) (init : 'a) (f : Type.t -> Type.t -> 'a -> t) : t =
  List.fold_left (fun acc (ty1, ty2) -> f ty1 ty2 acc) init cstr

let apply_subst (cstr : t) (st : Subst.t) : t =
  List.map (fun (ty1, ty2) -> (Subst.apply st ty1, Subst.apply st ty2)) cstr
