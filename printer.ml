open Hmx

let rec show_ty = function
    | TConst s -> s
    | TVar s ->
        let v = Union_find.find s in
        begin match v.structure with
            | Some s -> show_ty s
            | None -> v.name
        end
    | TApp (TApp (s1, s2), s3) when s1 = arrow ->
        Printf.sprintf "(%s -> %s)" (show_ty s2) (show_ty s3)
    | TApp (s1, s2) -> Printf.sprintf "(%s %s)" (show_ty s1) (show_ty s2)

let rec show_constr const = match const with
    | CDump -> "Dump"
    | CBool true -> "True"
    | CBool false -> "False"
    | CApp ("=", args) ->
        Printf.sprintf "%s = %s" (show_ty @@ List.hd args) (show_ty @@ List.hd @@ List.tl args)
    | CApp (pred, args) ->
        Printf.sprintf "%s %s"
            pred (String.concat ", " @@ List.map show_ty args)
    | CAnd (c1, c2) ->
        Printf.sprintf "(%s) ∧ (%s)" (show_constr c1) (show_constr c2)
    | CExists (vars, constr) ->
        Printf.sprintf "∃ %s : %s"
            (String.concat ", " (List.map (fun v -> (Union_find.find v).name) vars))
            (show_constr constr)
    | CDef (var, sch, constr) ->
        Printf.sprintf "def %s: %s in %s"
            var (show_sch sch) (show_constr constr)
    | CInstance (var, ty) ->
        Printf.sprintf "%s < %s"
            var (show_ty ty)

and show_sch (Forall (vars, constr, ty)) =
    if vars == [] then show_ty ty
    else Printf.sprintf "∀ %s [%s] : %s"
            (String.concat ", " (List.map (fun v -> (Union_find.find v).name) vars))
            (show_constr constr)
            (show_ty ty)
