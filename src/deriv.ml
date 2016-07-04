open Format

type judge = Env.t * Term.t * Type.aty [@@deriving to_yojson]

type judge_e = Env.t * Term.t * Type.t [@@deriving to_yojson]

type t =
  | Var of judge
  | Inter of judge_e * t * t
  | Abs_I of judge * t
  | Abs_K of judge * t
  | App of judge * t * t
  | F of judge_e * t * Type.evar
[@@deriving to_yojson]

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
  | F (je, skel', f) ->
      fprintf fmt "<%a:@[<v>%a@,%a@]>" Type.pp_evar f pp_judge_e je pp skel'

let rename_vars_judge (env, e, aty : judge) (s : string) : judge =
  (Env.rename_vars env s, e, Type.rename_vars_aty aty s)

let rename_vars_judge_e (env, e, ty : judge_e) (s: string) : judge_e =
  (Env.rename_vars env s, e, Type.rename_vars ty s)

let rec rename_vars (skel : t) (s : string) : t =
  match skel with
  | Var j ->
      Var (rename_vars_judge j s)
  | Inter (je, skel1, skel2) ->
      Inter (rename_vars_judge_e je s, rename_vars skel1 s, rename_vars skel2 s)
  | Abs_I (j, skel) ->
      Abs_I (rename_vars_judge j s, rename_vars skel s)
  | Abs_K (j, skel) ->
      Abs_K (rename_vars_judge j s, rename_vars skel s)
  | App (j, skel1, skel2) ->
      App (rename_vars_judge j s, rename_vars skel1 s, rename_vars skel2 s)
  | F (je, skel, f) ->
      F (rename_vars_judge_e je s, rename_vars skel s, Type.rename_evar f s)

let intersect (skel1 : t) (skel2 : t) : t =
  match skel1, skel2 with
  | (Var (env1, e1, aty1) | Abs_I ((env1, e1, aty1), _) |
     Abs_K ((env1, e1, aty1), _) | App ((env1, e1, aty1), _, _)),
    (Var (env2, e2, aty2) | Abs_I ((env2, e2, aty2), _) |
     Abs_K ((env2, e2, aty2), _) | App ((env2, e2, aty2), _, _))
    when e1 = e2 ->
      Inter ((Env.intersect env1 env2, e1, Type.(Inter (Lift aty1, Lift aty2))), skel1, skel2)
  | (Inter ((env1, e1, ty1), _, _) | F ((env1, e1, ty1), _, _)),
    (Inter ((env2, e2, ty2), _, _) | F ((env2, e2, ty2), _, _))
    when e1 = e2 ->
      Inter ((Env.intersect env1 env2, e1, Type.Inter (ty1, ty2)), skel1, skel2)
  | (Var (env1, e1, aty1) | Abs_I ((env1, e1, aty1), _) |
     Abs_K ((env1, e1, aty1), _) | App ((env1, e1, aty1), _, _)),
    (Inter ((env2, e2, ty2), _, _) | F ((env2, e2, ty2), _, _))
    when e1 = e2 ->
      Inter ((Env.intersect env1 env2, e1, Type.(Inter (Lift aty1, ty2))), skel1, skel2)
  | (Inter ((env1, e1, ty1), _, _) | F ((env1, e1, ty1), _, _)),
    (Var (env2, e2, aty2) | Abs_I ((env2, e2, aty2), _) |
     Abs_K ((env2, e2, aty2), _) | App ((env2, e2, aty2), _, _))
    when e1 = e2 ->
      Inter ((Env.intersect env1 env2, e1, Type.(Inter (ty1, Lift aty2))), skel1, skel2)
  | _ -> printf "%a@\n%a@\n" pp skel1 pp skel2; assert false

let applyF (skel : t) (f : Type.evar) : t =
  match skel with
  | (Var (env, e, aty) | Abs_I ((env, e, aty), _) |
     Abs_K ((env, e, aty), _) | App ((env, e, aty), _, _)) ->
      F ((Env.applyF env f, e, Type.(Expand (f, Lift aty))), skel, f)
  | (Inter ((env, e, ty), _, _) | F ((env, e, ty), _, _)) ->
      F ((Env.applyF env f, e, Type.Expand (f, ty)), skel, f)

let expand (skel : t) (ex : Type.ex) : t =
  let rec walk ex s =
    match ex with
    | Type.E_Hole -> rename_vars skel s
    | Type.E_Inter (ex1, ex2) -> intersect (walk ex1 (s ^ "0")) (walk ex2 (s ^ "1"))
    | Type.E_Expand (f, ex) -> applyF (walk ex s) f
  in
  walk ex ""

let replace_tvar_judge (env, e, aty : judge) (a : Type.tvar) (aty' : Type.aty) : judge =
  (Env.replace_tvar env a aty', e, Type.replace_tvar_aty aty a aty')

let replace_tvar_judge_e (env, e, ty : judge_e) (a : Type.tvar) (aty' : Type.aty) : judge_e =
  (Env.replace_tvar env a aty', e, Type.replace_tvar ty a aty')

let replace_evar_judge (env, e, aty : judge) (f : Type.evar) (ex : Type.ex) : judge =
  (Env.replace_evar env f ex, e, Type.replace_evar_aty aty f ex)

let replace_evar_judge_e (env, e, ty: judge_e) (f : Type.evar) (ex : Type.ex) : judge_e =
  (Env.replace_evar env f ex, e, Type.replace_evar ty f ex)

let rec replace_tvar (skel : t) (a : Type.tvar) (aty : Type.aty) : t =
  let judge x = replace_tvar_judge x a aty in
  let judge_e x = replace_tvar_judge_e x a aty in
  let loop x = replace_tvar x a aty in
  match skel with
  | Var j -> Var (judge j)
  | Inter (je, skel1, skel2) ->
      Inter (judge_e je, loop skel1, loop skel2)
  | Abs_I (j, skel) ->
      Abs_I (judge j, loop skel)
  | Abs_K (j, skel) ->
      Abs_K (judge j, loop skel)
  | App (j, skel1, skel2) ->
      App (judge j, loop skel1, loop skel2)
  | F (je, skel, f) ->
      F (judge_e je, loop skel, f)

let rec replace_evar (skel : t) (f : Type.evar) (ex : Type.ex) : t =
  let judge x = replace_evar_judge x f ex in
  let judge_e x = replace_evar_judge_e x f ex in
  let loop x = replace_evar x f ex in
  match skel with
  | Var j -> Var (replace_evar_judge j f ex)
  | Inter (je, skel1, skel2) ->
      Inter (judge_e je, loop skel1, loop skel2)
  | Abs_I (j, skel) ->
      Abs_I (judge j, loop skel)
  | Abs_K (j, skel) ->
      Abs_K (judge j, loop skel)
  | App (j, skel1, skel2) ->
      App (judge j, loop skel1, loop skel2)
  | F (je, skel, f') when f = f' ->
      expand skel ex
  | F (je, skel, f) ->
      F (judge_e je, loop skel, f)

let applySubst (skel : t) (st : Subst.t) : t =
  Subst.apply st replace_tvar replace_evar skel
