let rec generate (e : Term.t) : Type.aty * Env.t * Constraint.t * Deriv.t =
  match e with
  | Term.Var x ->
      let a = Type.fresh_tvar () in
      let aty = Type.Var a in
      let env = Env.extend Env.empty x (Type.Lift aty) in
      let cstr = [] in
      let skel = Deriv.Var (env, e, aty) in
      (aty, env, cstr, skel)
  | Term.App (e1, e2) ->
      let aty1, env1, cstr1, skel1 = generate e1 in
      let aty2, env2, cstr2, skel2 = generate e2 in
      let a = Type.fresh_tvar () in
      let f = Type.fresh_evar () in
      let aty = Type.Var a in
      let env = Env.(intersect env1 (applyF env2 f)) in
      let cstr = Constraint.(add (union cstr1 (applyF cstr2 f))
                               (Type.Lift aty1)
                               Type.(Lift (Arrow (Expand (f, Lift aty2), Var a))))
      in
      let skel = Deriv.(App ((env, e, aty), skel1, applyF skel2 f)) in
      (aty, env, cstr, skel)
  | Term.Abs (x, e') ->
      let aty', env', cstr', skel' = generate e' in
      let a = Type.fresh_tvar () in
      begin match Env.lookup env' x with
      | None ->
          let aty = Type.(Arrow (Lift (Var a), aty')) in
          let env = env' in
          let cstr = cstr' in
          let skel = Deriv.(Abs_K ((env, e, aty), skel')) in
          (aty, env, cstr, skel)
      | Some ty_dom ->
          let aty = Type.(Arrow (ty_dom, aty')) in
          let env = Env.remove env' x in
          let cstr = cstr' in
          let skel = Deriv.(Abs_I ((env, e, aty), skel')) in
          (aty, env, cstr, skel)
      end

let simplify (cstr : Constraint.t) : Constraint.t =
  let rec loop (ty1 : Type.t) (ty2 : Type.t) : Constraint.t =
    match ty1, ty2 with
    | Type.Expand (f1, ty1'), Type.Expand (f2, ty2')
      when f1 = f2 ->
        Constraint.applyF (loop ty1' ty2') f1
    | Type.Lift (Type.Arrow (ty11, aty12)), Type.Lift (Type.Arrow (ty21, aty22)) ->
        Constraint.union (loop ty21 ty11) (loop (Type.Lift aty12) (Type.Lift aty22))
    | Type.Inter (ty11, ty12), Type.Inter (ty21, ty22) ->
        Constraint.union (loop ty11 ty21) (loop ty12 ty22)
    | _ ->
        if ty1 = ty2 then Constraint.empty else Constraint.singleton ty1 ty2
  in
  Constraint.fold cstr Constraint.empty
    (fun ty1 ty2 cstr' -> Constraint.union cstr' (loop ty1 ty2))

let unify (cstr : Constraint.t) : Subst.t =
  let rec drop_leading_fs ty1 ty2 =
    match ty1, ty2 with
    | Type.Expand (f1, ty1'), Type.Expand (f2, ty2')
      when f1 = f2 ->
        drop_leading_fs ty1' ty2'
    | _ -> (ty1, ty2)
  in
  let rec loop (cstr : Constraint.t) (st : Subst.t) : Subst.t =
    match cstr with
    | [] -> st
    | (ty1, ty2) :: _ ->
        let ty1, ty2 = drop_leading_fs ty1 ty2 in
        let st' = match ty1, ty2 with
        | Type.Lift (Type.Var a), Type.Lift aty ->
            Subst.single_t a aty
        | Type.Lift aty, Type.Lift (Type.Var a) ->
            Subst.single_t a aty
        | Type.Expand (f, ty1), ty2 ->
            Subst.single_e f (Type.extract_expansion ty2)
        | _ ->
            Format.printf "failed to unify: @[%a@\n%a@]@\n" Type.pp ty1 Type.pp ty2;
            exit 1
        in
        loop (simplify (Constraint.applySubst cstr st')) (Subst.compose st st')
  in
  loop (simplify cstr) (Subst.empty)
