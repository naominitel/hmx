(**************************************************************************)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. Fran�ois Pottier, Yann R�gis-Gianas               *)
(*  and Didier R�my.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: constraint.ml 421 2006-12-22 09:27:42Z regisgia $ *)

open Positions
open Misc
open MultiEquation
open CoreAlgebra

(** [sname] is the type of the names that are used to refer to type
    schemes inside constraints. These names are bound by [CLet]
    constraints and referred to by [CInstance] constraints. *)
type sname = SName of string

(** [type_constraint] defines a syntax for the constraints between 
    types. *)
type ('crterm, 'variable) type_constraint =
    | CTrue of position
    | CDump of position
    | CEquation of position * 'crterm * 'crterm
    | CConjunction of ('crterm, 'variable) type_constraint list
    | CLet of ('crterm, 'variable) scheme list 
              * ('crterm, 'variable) type_constraint
    | CInstance of position * sname * 'crterm

(** A type scheme is a pair of a constraint [c] and a header [h],
    wrapped within two sets of universal quantifiers [rqs] and
    [fqs]. The former are considered rigid, while the latter are
    considered flexible. More precisely, for the type scheme to be
    considered consistent, the constraint [forall rqs.exists fqs.c]
    must hold. Rigid and flexible quantifiers otherwise play the same
    role, that is, they all end up universally quantified in the type
    scheme. A header is a mapping of names to types. *)
and ('crterm, 'variable) scheme =
  | Scheme of position * 'variable list * 'variable list 
              * ('crterm, 'variable) type_constraint * ('crterm * position) StringMap.t

type variable = MultiEquation.variable

type variable_kind = MultiEquation.variable_kind

type crterm = variable term

type tconstraint =
  (crterm, variable) type_constraint

type tscheme = 
  (crterm, variable) scheme

(* TEMPORARY ne pas oublier d'expliquer que les rangs et les pools ne sont
   pas toujours d'accord entre eux *)

(*
let rec expand_term = function
    | App (l, r) -> App (l, r)
    | _ -> assert false
   *)

(*
let rec expand_term_in_depth t = 
    let expand v = 
        let desc = UnionFind.find v in
        match desc.structure with
            | None -> TVariable v
            | Some t -> expand_term_in_depth t
    in
    TTerm (map expand t)
   *)

let rec cposition = function
    | CTrue pos -> 
        pos

    | CDump pos -> 
        pos

    | CLet ([], c) -> 
        cposition c

    | CConjunction [] -> 
        undefined_position

    | CConjunction l-> 
        join (cposition (List.hd l)) (cposition (last l))

    | CLet (l, _) ->
        join (sposition (List.hd l)) (sposition (last l))

    | CEquation (p, _, _) -> 
        p

    | CInstance (p, _, _) -> 
        p

and sposition = function 
    | Scheme (p, _, _, _, _) -> 
        p

(* TEMPORARY expliquer qu' on emploie la pile native pour les let 
   et conj. frames, plus des pools s'epar'es pour les let frames *)

(** [x <? t] is an instance constraint. *)
let (<?) x t pos =
    CInstance (pos, x, t)

(** [t1 =?= t2] is an equality constraint. *)
let (=?=) t1 t2 pos =
    CEquation (pos, t1, t2)

(** [c1 ^ c2] is a conjunction constraint. *)
let (^) c1 c2 =
    match c1, c2 with
        | CTrue _, c
        | c, CTrue _ ->
            c
        | c, CConjunction cl ->
            CConjunction (c :: cl)
        | _, _ ->
            CConjunction [c1; c2]

let conj cs = 
    List.fold_left ( ^ ) (CTrue undefined_position) cs

(** [ex qs c] returns the constraint [exists qs.c]. We encode existential
    constraints in terms of [let] constraints, since the latter are more
    general. *)
let ex ?pos qs c =
    CLet ([ Scheme (pos_or_undef pos, [], qs, c, StringMap.empty) ], 
          CTrue (pos_or_undef pos))

(** [fl qs c] returns the constraint [forall qs.c]. We encode universal
    constraints in terms of [let] constraints, since the latter are more
    general. *)
let fl ?pos qs c =
    CLet ([ Scheme (pos_or_undef pos, qs, [], c, StringMap.empty) ], 
          CTrue (pos_or_undef pos))

(** [exists f] creates a fresh variable [v] and returns the constraint
    [exists v.(f v)]. *)
let exists ?pos f =
    let v = variable Flexible () in
    let c = f (Var v) in
    ex ~pos:(pos_or_undef pos) [ v ] c

let exists3 ?pos f = 
    exists (fun x -> exists (fun y -> exists (fun z -> f x y z)))

(** [exists_list l f] associates a fresh variable with every element
    in the list [l], yielding an association list [m], and returns
    the constraint [exists m.(f m)]. *)
let exists_list ?pos l f =
    let l, m = variable_list Flexible l in
    ex ?pos:pos l (f m)

(** [forall_list l f] associates a fresh variable with every element
    in the list [l], yielding an association list [m], and returns
    the constraint [forall m.(f m)]. *)
let forall_list ?pos l f =
    let l, m = 
        List.fold_right (fun x (vs, xts) ->
            let v = variable Rigid ~name:x () in
            v :: vs, (x, Var v) :: xts
        ) l ([], []) 
    in
    fl ~pos:(pos_or_undef pos) l (f m)

(** [exists_set names f] associates a fresh variable with every name in
    the set [names], yielding a map [m] of names to variables, and returns
    the constraint [exists m.(f m)]. *)
let exists_set ?pos names f =
    let l, m = variable_set (const (Flexible, None)) names in
    ex ~pos:(pos_or_undef pos) l (f m)

(** [monoscheme header] turns [header] into a monomorphic type scheme. *)
let monoscheme ?pos header =
    Scheme (pos_or_undef pos, [], [], CTrue (pos_or_undef pos), header)

(** [scheme rqs names f] associates a fresh variable with every name in
    the set [names], yielding a map [m] of names to variables, and returns
    the type scheme [forall rqs m [f m] m], where the variables in [rqs]
    are rigid and the variables in [m] are flexible. *)
let scheme ?pos rqs names f =
    let l, m = variable_set (const (Flexible, None)) names in
    Scheme (pos_or_undef pos, rqs, l, f m, m)

(** [scheme' rqs rnames fnames f] associates a fresh variable with every 
    name in the set [fnames] and [rnames], yielding a map [m] of names to 
    variables, and returns the type scheme [forall (rqs @ rm) fm [f m] m], 
    where the variables in [rqs] and [rm] are rigid and the variables in [fm] 
    are flexible. *)
let scheme' ?pos rqs rnames fnames f =
    let fl, fm = variable_set (const (Flexible, None)) fnames in
    let rl, rm = variable_set (fun v -> (Rigid, Some v)) rnames in
    let m = map_union fm rm in
    Scheme (pos_or_undef pos, rqs @ rl, fl, f m, m)     

