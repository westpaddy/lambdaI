open Format

type t = (Term.var * Type.t) list [@@deriving to_yojson]

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
  let env2 = List.fold_left (fun env2 (x1, _) -> remove env2 x1) env2 env1 in
  env1 @ env2

let applyF (env : t) (f : Type.evar) : t =
  List.map (fun (x, ty) -> (x, Type.Expand (f, ty))) env
