open Hmx


let rec simpl_constr constr = match constr with
    | CAnd (c1, c2) ->
        begin match (simpl_constr c1, simpl_constr c2) with
            | CBool true, x -> x
            | x, CBool true -> x
                (*
            | CExists (c1, constr), x
            | x, CExists (c1, constr) ->
                (* TODO: α-convert c1 -> fresh var. *)
                CExists (c1, CAnd (constr, x))
*)
            | x, y -> CAnd (x, y)
        end
    | CExists ([], constr) -> simpl_constr constr
    | CExists (vars, constr) ->
        begin match simpl_constr constr with
            | CExists (v2, c) -> CExists (vars @ v2, c)
            | c -> CExists (vars, c)
        end
    | CDef (v, sch, constr) -> CDef (v, simpl_sch sch, simpl_constr constr)
    | _ -> constr


and simpl_sch (Forall (v, c, t)) =
    Forall (v, simpl_constr c, t)
