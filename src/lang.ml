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
  type tvar = int

  (** expansion variable: F *)
  type evar = int

  type aty =
    | Var of tvar               (* type variable: 'a *)
    | Arrow of t * aty          (* function type: t1->t2 *)

  and t =
    | Lift of aty               (* lifting of aty to type *)
    | Inter of t * t            (* intersection type: t1 /\ t2 *)
    | Expand of evar * t        (* expansion varaible application: Ft *)

  let rec pp_aty (fmt : formatter) (aty : aty) : unit =
    match aty with
    | Var x ->
        fprintf fmt "a%d" x
    | Arrow (ty, aty) ->
        fprintf fmt "@[(%a ->@ %a)@]" pp ty pp_aty aty

  and pp (fmt : formatter) (ty : t) : unit =
    match ty with
    | Lift aty ->
        pp_aty fmt aty
    | Inter (ty1, ty2) ->
        fprintf fmt "@[(%a /\\@ %a)@]" pp ty1 pp ty2
    | Expand (f, ty) ->
        fprintf fmt "(F%d %a)" f pp ty

  let tcounter = ref 0

  let fresh_tvar () : tvar =
    incr tcounter; !tcounter

  let ecounter = ref 0

  let fresh_evar () : evar =
    incr ecounter; !ecounter
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

module Constraint = struct
  type t = (Type.t * Type.t) list

  let pp (fmt : formatter) (cstr : t) : unit =
    fprintf fmt "{@[<v>%a@]}"
      (pp_print_list
         (fun fmt (ty1, ty2) -> fprintf fmt "%a = %a" Type.pp ty1 Type.pp ty2))
      cstr

  let singleton (ty1 : Type.t) (ty2 : Type.t) : t =
    [ty1, ty2]

  let add (cstr : t) (ty1 : Type.t) (ty2 : Type.t) : t =
    if List.mem (ty1, ty2) cstr then cstr else (ty1, ty2) :: cstr

  let union (cstr1 : t) (cstr2 : t) : t =
    List.fold_left (fun cstr (ty1, ty2) -> add cstr ty1 ty2) cstr1 cstr2

  let applyF (cstr : t) (f : Type.evar) : t =
    List.map (fun (ty1, ty2) -> (Type.Expand (f, ty1), Type.Expand (f, ty2))) cstr
end
