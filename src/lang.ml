open Format

module Term = struct
  type var = string             (* lambda variable: x *)

  type t =
    | Var of var                (* lambda variable: x *)
    | App of t * t              (* application: e1 e2 *)
    | Abs of var * t            (* abstraction: \x.e *)

  let rec pp (fmt : formatter) (e : t) : unit =
    match e with
    | Var x ->
        fprintf fmt "%s" x
    | App (e1, e2) ->
        fprintf fmt "@[(%a@ %a)@]" pp e1 pp e2
    | Abs (x, e) ->
        fprintf fmt "@[(fun %s ->@ %a)@]" x pp e
end

module Type = struct
  (** type variable: 'a *)
  type tvar = int * string

  (** expansion variable: F *)
  type evar = int * string

  type aty =
    | Var of tvar               (* type variable: 'a *)
    | Arrow of t * aty          (* function type: t1->t2 *)

  and t =
    | Lift of aty               (* lifting of aty to type *)
    | Inter of t * t            (* intersection type: t1 /\ t2 *)
    | Expand of evar * t        (* expansion varaible application: Ft *)

  let pp_tvar (fmt : formatter) (tvar : tvar) : unit =
    match tvar with
    | d, "" ->
        fprintf fmt "'a%d" d
    | d, l ->
        fprintf fmt "'a%d\"%s\"" d l

  let pp_evar (fmt : formatter) (evar : evar) : unit =
    match evar with
    | f, "" ->
        fprintf fmt "F%d" f
    | f, l ->
        fprintf fmt "F%d\"%s\"" f l

  let rec pp_aty (fmt : formatter) (aty : aty) : unit =
    match aty with
    | Var x ->
        fprintf fmt "%a" pp_tvar x
    | Arrow (ty, aty) ->
        fprintf fmt "@[(%a ->@ %a)@]" pp ty pp_aty aty

  and pp (fmt : formatter) (ty : t) : unit =
    match ty with
    | Lift aty ->
        pp_aty fmt aty
    | Inter (ty1, ty2) ->
        fprintf fmt "@[(%a /\\@ %a)@]" pp ty1 pp ty2
    | Expand (f, ty) ->
        fprintf fmt "(%a %a)" pp_evar f pp ty

  let tcounter = ref 0

  let fresh_tvar () : tvar =
    incr tcounter; (!tcounter, "")

  let ecounter = ref 0

  let fresh_evar () : evar =
    incr ecounter; (!ecounter, "")
end

module Env = struct
  type t = (Term.var * Type.t) list

  let pp (fmt : formatter) (env : t) : unit =
    fprintf fmt "{@[<v>%a@]}"
      (pp_print_list (fun fmt (x, ty) -> fprintf fmt "%s : %a" x Type.pp ty))
      env

  (** An empty environment *)
  let empty = []

  (** [lookup env x] returns an associated type with lambda variable [x].  If no
      such type, this returns [None]. *)
  let lookup (env : t) (x : Term.var) : Type.t option =
    try Some (List.assoc x env) with Not_found -> None

  (** [extend env x t] extends [env] with association [t] to [x]. *)
  let extend (env : t) (x : Term.var) (ty : Type.t) : t =
    (x, ty) :: env

  (** [remove env x] removes all associations concerning [x] from [env], i.e.,
      [lookup (remove env x) x] always returns [None]. *)
  let remove (env : t) (x : Term.var) : t =
    List.filter (fun (x', _) -> x' <> x) env

  let intersect (env1 : t) (env2 : t) : t =
    let env1 = env1 |> List.map (fun (x1, t1) ->
        match lookup env2 x1 with
        | None -> (x1, t1)
        | Some t2 -> (x1, Type.Inter (t1, t2)))
    in
    env1 @ env2

  let applyF (env : t) (f : Type.evar) : t =
    List.map (fun (x, ty) -> (x, Type.Expand (f, ty))) env
end

module Skel = struct
  type judge = Env.t * Term.t * Type.aty

  type judge_e = Env.t * Term.t * Type.t

  type t =
    | Var of judge
    | Inter of judge_e * t * t
    | Abs_I of judge * t
    | Abs_K of judge * t
    | App of judge * t * t
    | F of judge_e * t

  let pp_judge (fmt : formatter) (env, e, aty : judge) : unit =
    fprintf fmt "@[%a |-@ %a :@ %a@]" Env.pp env Term.pp e Type.pp_aty aty

  let pp_judge_e (fmt : formatter) (env, e, ty : judge_e) : unit =
    fprintf fmt "@[%a |-@ %a :@ %a@]" Env.pp env Term.pp e Type.pp ty

  let rec pp (fmt : formatter) (skel : t) : unit =
    match skel with
    | Var j ->
        fprintf fmt "<VAR:%a>" pp_judge j
    | Inter (je, skel1, skel2) ->
        fprintf fmt "</\\:@[<v>%a@,%a@,%a@]>" pp_judge_e je pp skel1 pp skel2
    | Abs_I (j, skel') ->
        fprintf fmt "<ABS_I:@[<v>%a@,%a@]>" pp_judge j pp skel'
    | Abs_K (j, skel') ->
        fprintf fmt "<ABS_K:@[<v>%a@,%a@]>" pp_judge j pp skel'
    | App (j, skel1, skel2) ->
        fprintf fmt "<APP:@[<v>%a@,%a@,%a@]>" pp_judge j pp skel1 pp skel2
    | F (je, skel') ->
        fprintf fmt "<F:@[<v>%a@,%a@]>" pp_judge_e je pp skel'

  let applyF (skel : t) (f : Type.evar) : t =
    match skel with
    | (Var (env, e, aty) | Abs_I ((env, e, aty), _) |
       Abs_K ((env, e, aty), _) | App ((env, e, aty), _, _)) ->
        F ((Env.applyF env f, e, Type.(Expand (f, Lift aty))), skel)
    | (Inter ((env, e, ty), _, _) | F ((env, e, ty), _)) ->
        F ((Env.applyF env f, e, Type.Expand (f, ty)), skel)
end

module Expansion = struct
  type t =
    | Inter of t * t
    | Expand of Type.evar * t
    | Hole

  let rec pp (fmt : formatter) (ex : t) : unit =
    match ex with
    | Inter (ex1, ex2) -> fprintf fmt "(%a /\\@ %a)" pp ex1 pp ex2
    | Expand (f, ex) -> fprintf fmt "(%a %a)" Type.pp_evar f pp ex
    | Hole -> fprintf fmt "[]"

  let rec extract_from_type (ty : Type.t) : t =
    match ty with
    | Type.Lift _ -> Hole
    | Type.Inter (ty1, ty2) -> Inter (extract_from_type ty1, extract_from_type ty2)
    | Type.Expand (f, ty) -> Expand (f, extract_from_type ty)
end

module TypeCtx = struct
  type atc =
    | Var of Type.tvar
    | Arrow of t * atc

  and t =
    | Lift of atc
    | Inter of t * t
    | Expand of Type.evar * t
    | Hole

  let rec pp_atc (fmt : formatter) (atc : atc) : unit =
    match atc with
    | Var a -> fprintf fmt "%a" Type.pp_tvar a
    | Arrow (tc, atc) -> fprintf fmt "(%a ->@ %a)" pp tc pp_atc atc

  and pp (fmt : formatter) (tc : t) : unit =
    match tc with
    | Lift atc -> fprintf fmt "%a" pp_atc atc
    | Inter (tc1, tc2) -> fprintf fmt "(%a /\\@ %a)" pp tc1 pp tc2
    | Expand (f, tc) -> fprintf fmt "(%a %a)" Type.pp_evar f pp tc
    | Hole -> fprintf fmt "[]"

  let rec of_expansion (e : Expansion.t) : t =
    match e with
    | Expansion.Inter (e1, e2) ->
        Inter (of_expansion e1, of_expansion e2)
    | Expansion.Expand (f, e) ->
        Expand (f, of_expansion e)
    | Expansion.Hole -> Hole

  let rec of_aty (aty : Type.aty) : atc =
    match aty with
    | Type.Var a ->
        Var a
    | Type.Arrow (ty, aty) ->
        Arrow (of_type ty, of_aty aty)

  and of_type (ty : Type.t) : t =
    match ty with
    | Type.Lift aty ->
        Lift (of_aty aty)
    | Type.Inter (ty1, ty2) ->
        Inter (of_type ty1, of_type ty2)
    | Type.Expand (f, ty) ->
        Expand (f, of_type ty)

  let rec nb_holes_atc (atc : atc) : int =
    match atc with
    | Var _ -> 0
    | Arrow (tc, atc) -> nb_holes tc + nb_holes_atc atc

  and nb_holes (tc : t) : int =
    match tc with
    | Lift atc -> nb_holes_atc atc
    | Inter (tc1, tc2) -> nb_holes tc1 + nb_holes tc2
    | Expand (_, tc) -> nb_holes tc
    | Hole -> 1

  let rec paths_atc (atc : atc) : string list =
    match atc with
    | Var _ -> []
    | Arrow (tc, atc) ->
        List.map ((^) "L") (paths tc) @ List.map ((^) "R") (paths_atc atc)

  and paths (tc : t) : string list =
    match tc with
    | Lift atc -> paths_atc atc
    | Inter (tc1, tc2) ->
        List.map ((^) "0") (paths tc1) @ List.map ((^) "1") (paths tc2)
    | Expand (f, tc) -> paths tc
    | Hole -> [""]

  let rec rename_var_atc (atc : atc) (suffix : string) : atc =
    match atc with
    | Var (d, l) -> Var (d, l ^ suffix)
    | Arrow (tc, atc) -> Arrow (rename_var tc suffix, rename_var_atc atc suffix)

  and rename_var (tc : t) (suffix : string) : t =
    match tc with
    | Lift atc -> Lift (rename_var_atc atc suffix)
    | Inter (tc1, tc2) -> Inter (rename_var tc1 suffix, rename_var tc2 suffix)
    | Expand ((d, l), tc) -> Expand ((d, l ^ suffix), rename_var tc suffix)
    | Hole -> Hole

  let rec fill_holes_atc (atc : atc) (tcs : t list) : atc * t list =
    match atc with
    | Var a -> (Var a, tcs)
    | Arrow (tc, atc) ->
        let tc', tcs' = fill_holes tc tcs in
        let atc', tcs'' = fill_holes_atc atc tcs' in
        (Arrow (tc', atc'), tcs'')

  and fill_holes (tc : t) (tcs : t list) : t * t list =
    match tc with
    | Lift atc ->
        let atc', tcs' = fill_holes_atc atc tcs in
        (Lift atc', tcs')
    | Inter (tc1, tc2) ->
        let tc1', tcs' = fill_holes tc1 tcs in
        let tc2', tcs'' = fill_holes tc2 tcs' in
        (Inter (tc1', tc2'), tcs'')
    | Expand (f, tc) ->
        let tc', tcs' = fill_holes tc tcs in
        (Expand (f, tc'), tcs')
    | Hole ->
        begin match tcs with
        | [] -> failwith (sprintf "%s fill types list too short" __LOC__)
        | tc :: tcl -> (tc, tcl)
        end

  let rec subst_tvar_atc (atc : atc) (a : Type.tvar) (aty : Type.aty) : atc =
    match atc with
    | Var a' -> if a = a' then of_aty aty else Var a'
    | Arrow (tc, atc) -> Arrow (subst_tvar tc a aty, subst_tvar_atc atc a aty)

  and subst_tvar (tc : t) (a : Type.tvar) (aty : Type.aty) : t =
    match tc with
    | Lift atc -> Lift (subst_tvar_atc atc a aty)
    | Inter (tc1, tc2) -> Inter (subst_tvar tc1 a aty, subst_tvar tc2 a aty)
    | Expand (f, tc) -> Expand (f, subst_tvar tc a aty)
    | Hole -> Hole

  let rec subst_evar_atc (atc : atc) (f : Type.evar) (ex : Expansion.t) : atc =
    match atc with
    | Var a -> Var a
    | Arrow (tc, atc) -> Arrow (subst_evar tc f ex, subst_evar_atc atc f ex)

  and subst_evar (tc : t) (f : Type.evar) (ex : Expansion.t) : t =
    match tc with
    | Lift atc -> Lift (subst_evar_atc atc f ex)
    | Inter (tc1, tc2) -> Inter (subst_evar tc1 f ex, subst_evar tc2 f ex)
    | Expand (f', tc) ->
        if f = f' then
          let tc_skel = of_expansion ex in
          let paths = paths tc_skel in
          let tc, _ = fill_holes tc_skel (List.map (fun s -> rename_var tc s) paths) in
          tc
        else
          Expand (f', subst_evar tc f ex)
    | Hole -> Hole
end

module Subst = struct
  type replacer =
    | T of Type.tvar * Type.aty
    | E of Type.evar * Expansion.t

  type t = replacer list

  let pp_replacer (fmt : formatter) (r : replacer) : unit =
    match r with
    | T (a, aty) -> fprintf fmt "%a := %a" Type.pp_tvar a Type.pp_aty aty
    | E (f, ex) -> fprintf fmt "%a := %a" Type.pp_evar f Expansion.pp ex

  let pp (fmt : formatter) (st : t) : unit =
    fprintf fmt "{|@[<v>%a@]|}" (pp_print_list pp_replacer) st

  let empty = []

  let single_t (a : Type.tvar) (aty : Type.aty) : t =
    [T (a, aty)]

  let single_e (f : Type.evar) (ex : Expansion.t) : t =
    [E (f, ex)]

  let compose (st1 : t) (st2 : t) : t =
    st1 @ st2

  let apply (st : t) (tc : TypeCtx.t) : TypeCtx.t =
    List.fold_left
      (fun tc r ->
         match r with
         | T (a, aty) -> TypeCtx.subst_tvar tc a aty
         | E (f, ex) -> TypeCtx.subst_evar tc f ex)
      tc
      st
end

module Constraint = struct
  type t = (Type.t * Type.t) list

  let pp (fmt : formatter) (cstr : t) : unit =
    fprintf fmt "{@[<v>%a@]}"
      (pp_print_list
         (fun fmt (ty1, ty2) -> fprintf fmt "%a = %a" Type.pp ty1 Type.pp ty2))
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
    List.map (fun (ty1, ty2) -> (Subst.apply
end
