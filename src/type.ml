open Format

type tvar = int * string [@@deriving to_yojson]

let pp_tvar (fmt : formatter) (tvar : tvar) : unit =
  match tvar with
  | d, "" -> fprintf fmt "'a%d" d
  | d, l -> fprintf fmt "'a%d\"%s\"" d l

type evar = int * string [@@deriving to_yojson]

let pp_evar (fmt : formatter) (evar : evar) : unit =
  match evar with
  | f, "" -> fprintf fmt "F%d" f
  | f, l -> fprintf fmt "F%d\"%s\"" f l

type aty =
  | Var of tvar
  | Arrow of t * aty

and t =
  | Lift of aty
  | Inter of t * t
  | Expand of evar * t
[@@deriving to_yojson]

let rec pp_aty (fmt : formatter) (aty : aty) : unit =
  match aty with
  | Var a -> pp_tvar fmt a
  | Arrow (ty, aty) -> fprintf fmt "(%a ->@ %a)" pp ty pp_aty aty

and pp (fmt : formatter) (ty : t) : unit =
  match ty with
  | Lift aty -> fprintf fmt "%a" pp_aty aty
  | Inter (ty1, ty2) -> fprintf fmt "(%a /\\@ %a)" pp ty1 pp ty2
  | Expand (f, ty) -> fprintf fmt "(%a %a)" pp_evar f pp ty

let tcounter = ref 0

let fresh_tvar () : tvar =
  incr tcounter; (!tcounter, "")

let ecounter = ref 0

let fresh_evar () : evar =
  incr ecounter; (!ecounter, "")

let rec rename_vars_aty (aty : aty) (s : string) : aty =
  match aty with
  | Var (d, l) -> Var (d, l ^ s)
  | Arrow (ty, aty) -> Arrow (rename_vars ty s, rename_vars_aty aty s)

and rename_vars (ty : t) (s : string) : t =
  match ty with
  | Lift aty -> Lift (rename_vars_aty aty s)
  | Inter (ty1, ty2) -> Inter (rename_vars ty1 s, rename_vars ty2 s)
  | Expand ((d, l), ty) -> Expand ((d, l ^ s), rename_vars ty s)

type ex =
  | E_Hole
  | E_Inter of ex * ex
  | E_Expand of evar * ex

let rec pp_ex (fmt : formatter) (ex : ex) : unit =
  match ex with
  | E_Hole -> pp_print_string fmt "[]"
  | E_Inter (ex1, ex2) -> fprintf fmt "(%a /\\@ %a)" pp_ex ex1 pp_ex ex2
  | E_Expand (f, ex) -> fprintf fmt "(%a %a)" pp_evar f pp_ex ex

let rec extract_expansion (ty : t) : ex =
  match ty with
  | Lift _ -> E_Hole
  | Inter (ty1, ty2) -> E_Inter (extract_expansion ty1, extract_expansion ty2)
  | Expand (f, ty) -> E_Expand (f, extract_expansion ty)

let rec expand (ty : t) (ex : ex) : t =
  let rec walk ex s =
    match ex with
    | E_Hole -> rename_vars ty s
    | E_Inter (ex1, ex2) -> Inter (walk ex1 (s ^ "0"), walk ex2 (s ^ "1"))
    | E_Expand (f, ex) -> Expand (f, walk ex s)
  in
  walk ex ""

let rec replace_tvar_aty (aty : aty) (a : tvar) (aty' : aty) : aty =
  match aty with
  | Var a' -> if a = a' then aty' else aty
  | Arrow (ty, aty) -> Arrow (replace_tvar ty a aty', replace_tvar_aty aty a aty')

and replace_tvar (ty : t) (a : tvar) (aty' : aty) : t =
  match ty with
  | Lift aty -> Lift (replace_tvar_aty aty a aty')
  | Inter (ty1, ty2) -> Inter (replace_tvar ty1 a aty', replace_tvar ty2 a aty')
  | Expand (f, ty) -> Expand (f, replace_tvar ty a aty')

let rec replace_evar_aty (aty : aty) (f : evar) (ex : ex) : aty =
  match aty with
  | Var _ -> aty
  | Arrow (ty, aty) -> Arrow (replace_evar ty f ex, replace_evar_aty aty f ex)

and replace_evar (ty : t) (f : evar) (ex : ex) : t =
  match ty with
  | Lift aty -> Lift (replace_evar_aty aty f ex)
  | Inter (ty1, ty2) -> Inter (replace_evar ty1 f ex, replace_evar ty2 f ex)
  | Expand (f', ty') -> if f = f' then expand ty' ex else ty
