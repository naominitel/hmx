open Hmx
open Printer

let extend env var sch = (var, sch) :: env
let lookup env var = List.assoc var env

let chop term =
    let var = fresh_ty_var () in
    (Union_find.find var).structure <- Some term ;
    var

let rec alpha_conv term name r = match term with
    | TApp (t1, t2) -> TApp (alpha_conv t1 name r, alpha_conv t2 name r)
    | TVar v ->
        let var = Union_find.find v in
        begin match var.structure with
            | Some s -> alpha_conv s name r
            | None -> if var.name = name then r else term
        end
    | TConst _ -> term

let instance (Forall (vars, c, term)) =
    List.fold_left
        (fun term v ->
             alpha_conv term (Union_find.find v).name @@ TVar (fresh_ty_var ()))
        term vars

let rec unify t1 t2 =
    let open Union_find in
    match ((find t1).structure, (find t2).structure) with
        | None, None ->
            union (fun x y -> if x.rank < y.rank then x else y) t1 t2
        | (Some r1, None)    ->
            union (fun x y -> if x.rank < y.rank then x else {y with structure = x.structure}) t1 t2
        | (None, Some r2)    ->
            union (fun x y -> if x.rank < y.rank then {x with structure = y.structure} else y) t1 t2
        | (Some r1, Some r2) ->
            union (fun x y -> if x.rank < y.rank then x else y) t1 t2 ; unify_terms r1 r2

and unify_terms t1 t2 =
    match (t1, t2) with
        | (TVar v1, TVar v2) -> unify v1 v2
        | (TVar v, t)
        | (t, TVar v) -> unify v (chop t)
        | (TApp (t1, v1), TApp(t2, v2)) -> unify_terms t1 t2 ; unify_terms v1 v2
        | (TConst x, TConst y) when x = y -> ()
        | _ -> failwith (Printf.sprintf "impossible to unify %s with %s"
                        (show_ty t1) (show_ty t2))

type pool = {
    mutable vars: var list ;
    mutable rank: int
}

let rec free_vars_of term = match term with
    | TConst _ -> []
    | TVar v ->
        begin match (Union_find.find v).structure with
            | Some s -> free_vars_of s
            | None -> [v]
        end
    | TApp (t1, t2) -> (free_vars_of t1 @ free_vars_of t2)

let rec solve constr pool env =
    match constr with
        | CDump -> env
        | CBool true -> []
        | CBool false -> failwith "false"
        | CApp ("=", [t1 ; t2]) -> unify (chop t1) (chop t2) ; []
        | CApp _ -> failwith "bad predicate application"
        | CAnd (c1, c2) -> ignore @@ solve c1 pool env ; solve c2 pool env
        | CExists (vars, constr) ->
            List.iter (fun var -> (Union_find.find var).Hmx.rank <- pool.rank) vars ;
            pool.vars <- vars @ pool.vars ;
            solve constr pool env
        | CDef (var, sch, constr) ->
            let (Forall (vars, c, t)) = sch in
            let v = chop t in

            let pool' = { rank = pool.rank + 1 ; vars = [] } in
            List.iter (fun var -> (Union_find.find var).Hmx.rank <- pool'.rank) vars ;
            pool.vars <- vars @ pool'.vars ;

            ignore @@ solve c pool' env ;

            let t = match (Union_find.find v).structure with
                | Some s -> s
                | None -> TVar v
            in

            (* we only generalize up to the rank of this pool... *)
            let vars =
                List.filter
                    (fun v -> (Union_find.find v).Hmx.rank > pool.rank)
                    (free_vars_of t)
            in solve constr pool (extend env var (Forall (vars, CBool true, t)))
        | CInstance (var, ty) ->
            try unify_terms ty @@ instance (lookup env var) ; []
            with Not_found -> failwith "unbound variable"

let run const = solve const { rank = 0 ; vars = [] } []
