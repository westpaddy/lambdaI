open Format

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
