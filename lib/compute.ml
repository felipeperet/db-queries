open Base
open Datatypes

(* Numerical computations *)

(* infix composition operator *)
let ( ++ ) f g x = g (f x)

(* compute the total amount of received fees from a given set of cards *)
let total db dcs =
  List.fold_right
    ~f:(field db "Amount" ++ Float.of_string ++ ( +. ))
    dcs ~init:0.0
