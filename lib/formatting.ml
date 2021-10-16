open Base

open Datatypes

(* function that formats the splitting of a line using a given separating character: *)
let format_list c =
  let s = String.make 1 c in
  List.fold_left ~f:(fun x y -> if (String.equal x "") then y else x^s^y)  ~init:""

(* returns the fields associated with a given list of names in a given card *)
let extract db ns dc =
  List.map ~f:(fun n -> field db n dc) ns

(* formatting *)
let format_line db ns dc =
  (String.uppercase (field db "Lastname" dc))
  ^" "^(field db "Firstname" dc)
  ^"\t"^(format_list '\t' (extract db ns dc))
  ^"\n"
