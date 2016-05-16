type const =
  | CInt of int
  | CBool of bool

type term =
  | Var of string
  | App of term * term
  | Abs of string * term
  | Let of string * term * term
  | Const of const

type ty =
    | TConst of string
    | TVar of var
    | TApp of ty * ty

and var = var_descr Union_find.point

and var_descr = {
    mutable structure: ty option ;
    mutable rank: int ;
    name: string
}

let arrow = TConst "->"

let function_type t1 t2 =
    TApp ((TApp (arrow, t1)), t2)

type ty_sch =
  | Forall of var list * constr * ty

and constr =
    | CBool of bool
    | CApp of string * ty list
    | CAnd of constr * constr
    | CExists of var list * constr
    | CDef of string * ty_sch * constr
    | CInstance of string * ty
    | CDump

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
    CDef (var, sch, constr)

let fresh_ty_var =
    let next = ref 0 in
    fun () ->
        let name = Printf.sprintf "α%d" !next in
        incr next ;
        Union_find.fresh {
            structure = None ;
            name = name ;
            rank = 0
        }

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
    | Let (z, e, t) ->
        let x = fresh_ty_var () in
        letin z (Forall ([x], infer e (TVar x), TVar x)) (infer t ty)
    | App (f, a) ->
        let x2 = fresh_ty_var () in
        CExists ([x2], CAnd (infer f (function_type (TVar x2) ty),
                             infer a (TVar x2)))

type def = Def of string * term
type prog = def list

let infer_prog p =
    List.fold_right
        (fun (Def (v, t)) acc ->
             let x = fresh_ty_var () in
             letin v (Forall ([x], infer t (TVar x), TVar x)) acc)
        p CDump
