open Hmx

let t0 = App (Abs ("x", Var "x"), Const (CInt 0))
let t1 = Let ("f", Const (CInt 0), Var "f")
let t3 = Let ("f", Abs ("x", Var "x"), Var "f")
let t2 = Let ("f", Abs ("x", Var "x"), App (Var "f", Const (CInt 0)))
let t4 = Let ("f", Abs ("x", Var "x"), (Let ("y", Var "f", App (Var "y", Const (CInt 0)))))

let t = fresh_ty_var ()
let constr = infer t4 (TVar t)

let prog = [
    Def ("t0", Const (CInt 0)) ;
    Def ("t1", Abs ("x", Var "x")) ;
    Def ("t2", App (Var "t1", Var "t0")) ;
    Def ("t3", App (Var "t1", Var "t1")) ;
    Def ("t4", Var "t3") ;
    Def ("t5", Abs ("x", Abs ("y", App (Var "x", Var "y")))) ;
    Def ("t6", App (Var "t5", Var "t5"))
]

let () =
    List.iter
        (fun (v, sch) ->
             Printf.printf "%s: %s\n" v (Printer.show_sch sch))
        @@ List.rev @@ Solver.run (infer_prog prog)
