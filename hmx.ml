type const =
  | CInt of int
  | CBool of bool

type term =
  | Var of string
  | App of term * term
  | Abs of string * term
  | Const of const

type ty =
  | TConst of string
  | TVar of string
  | TApp of ty * ty

let arrow = TConst "->"

let function_type t1 t2 =
    TApp ((TApp (arrow, t1)), t2)

type ty_sch =
  | Forall of string list * constr * ty

and constr =
    | CBool of bool
    | CApp of string * ty list
    | CAnd of constr * constr
    | CExists of string list * constr
    | CDef of string * ty_sch * constr
    | CInstance of string * ty

let sch ty =
    Forall ([], CBool true, ty)

let is_subtype = "="

let is_instance sch ty =
    let Forall(v, c, t) = sch in
    CExists (v, CAnd (c, CApp (is_subtype, [t ; ty])))

let has_instance sch =
    let Forall(v, c, _) = sch in
    CExists (v, c)

let letin var sch constr =
    CAnd (has_instance sch, CDef (var, sch, constr))

let fresh_ty_var =
    let next = ref 0 in
    fun () ->
        let ret = !next in
        incr next ;
        Printf.sprintf "α%d" ret

let t_int = TConst "int"
let t_bool = TConst "bool"

let rec infer term ty = match term with
    | Const (CInt _) -> CApp (is_subtype, [t_int ; ty])
    | Const (CBool _) -> CApp (is_subtype, [t_bool ; ty])
    | Var x -> CInstance (x, ty)
    | Abs (x, t) ->
        let x1 = fresh_ty_var () in
        let x2 = fresh_ty_var () in
        let constr_body = infer t (TVar x2) in
        CExists ([x1 ; x2], CAnd (CDef (x, sch (TVar x1), constr_body),
                                  CApp (is_subtype, [function_type (TVar x1) (TVar x2) ; ty])))
    | App (Abs(z, t), e) ->
        (* let x = e in t *)
        let x = fresh_ty_var () in
        letin z (Forall ([x], infer e (TVar x), TVar x)) (infer t ty)
    | App (f, a) ->
        let x2 = fresh_ty_var () in
        CExists ([x2], CAnd (infer f (function_type (TVar x2) ty),
                             infer a (TVar x2)))

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

let rec show_ty = function
    | TConst s -> s
    | TVar s -> s
    | TApp (TApp (s1, s2), s3) when s1 = arrow ->
        Printf.sprintf "%s -> %s" (show_ty s2) (show_ty s3)
    | TApp (s1, s2) -> Printf.sprintf "(%s %s)" (show_ty s1) (show_ty s2)

let rec show_constr const = match const with
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
            (String.concat ", " vars)
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
            (String.concat ", " vars)
            (show_constr constr)
            (show_ty ty)

module VMap = Map.Make(struct type t = string let compare = Pervasives.compare end)
type env = ty_sch VMap.t

let rec solve env pool constr = match constr with
    | CBool (true) -> env
    | CBool (false) -> failwith "false"
    | CApp (pred, [t1 ; t2]) when pred = is_subtype ->
        
    |
(*let rec solve env pool constr = match constr with*)


let term_let v e b = App (Abs (v, b), e)

let main =
    term_let
        "x" (Abs ("y", Abs ("z", App (Var "y", Var "z"))))
        (App (Var "x", Var "x"))

let t1 = term_let "x" (Const (CInt 0)) (Var "x")

let constr = infer t1 (TVar (fresh_ty_var ()))

let () =
    Printf.printf "%s\n" @@ show_constr @@ simpl_constr constr
