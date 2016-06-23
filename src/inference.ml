open Lang

let rec generate (e : Term.t) : Type.aty * Env.t * Constraint.t * Skel.t =
  match e with
  | Term.Var x ->
      let a = Type.fresh_tvar () in
      let aty = Type.Var a in
      let env = Env.extend Env.empty x (Type.Lift aty) in
      let cstr = [] in
      let skel = Skel.Var (env, e, aty) in
      (aty, env, cstr, skel)
  | Term.App (e1, e2) ->
      let aty1, env1, cstr1, skel1 = generate e1 in
      let aty2, env2, cstr2, skel2 = generate e2 in
      let a = Type.fresh_tvar () in
      let f = Type.fresh_evar () in
      let aty = Type.Var a in
      let env = Env.(intersect env1 (applyF env2 f)) in
      let cstr = Constraint.(add (union cstr1 (applyF cstr2 f))
                               (Type.Lift aty1) Type.(Lift (Arrow (Lift aty2, Var a))))
      in
      let skel = Skel.(App ((env, e, aty), skel1, applyF skel2 f)) in
      (aty, env, cstr, skel)
  | Term.Abs (x, e') ->
      let aty', env', cstr', skel' = generate e' in
      let a = Type.fresh_tvar () in
      begin match Env.lookup env' x with
      | None ->
          let aty = Type.(Arrow (Lift (Var a), aty')) in
          let env = env' in
          let cstr = cstr' in
          let skel = Skel.(Abs_K ((env, e, aty), skel')) in
          (aty, env, cstr, skel)
      | Some ty_dom ->
          let aty = Type.(Arrow (ty_dom, aty')) in
          let env = Env.remove env' x in
          let cstr = cstr' in
          let skel = Skel.(Abs_I ((env, e, aty), skel')) in
          (aty, env, cstr, skel)
      end
