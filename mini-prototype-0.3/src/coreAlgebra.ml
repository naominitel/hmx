(**************************************************************************)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
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

(* $Id: coreAlgebra.ml 421 2006-12-22 09:27:42Z regisgia $ *)

open Misc
open Positions

(** The terms of a row algebra include a binary row extension
    constructor for every row label, the unary constant row
    constructor, and the terms of the underlying free algebra. *)
type 'a term =
  | App of 'a term * 'a term
  | Var of 'a

let rec iter f = function
    | App (l, r) -> iter f l ; iter f r
    | Var v -> f v

let rec map f = function
    | App (l, r) -> App (map f l, map f r)
    | Var v -> Var (f v)

let rec fold f term accu =
    match term with
        | App (l, r) -> f (fold f r accu) (fold f l accu)
        | Var v -> f v accu

(*
let rec fold2 f term term' accu =
    match term, term' with
        | App (l, r), App (l', r') -> f r r' (f l l' accu)
        | Var v, Var v' -> accu
        | _ -> failwith "fold2"
   *)

let app t args = List.fold_left (fun acu x -> App (acu, x)) t args
