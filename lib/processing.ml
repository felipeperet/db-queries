open Base

open Datatypes

(*  We decide to represent dates in a card as a string with format dd.mm.yyyy.
 *  In order to be able to define additional comparisons, we also allow the replacement of the day,
 *  month or year part with the underscore character ('_').
 *
 *  Dates are compared according to the lexicographic order of lists of integers of the form [year; month; day].
 *  To express queries such as: ``is before July 1998'', we use the date pattern: "_.07.1998".
 *
 *  Comparing a date with a pattern is accomplished with
 *  the function tst_dfield which analyses the pattern to create the ad hoc comparison function.
 *
 *  To define this generic test function on dates, we need a few auxiliary functions. *)

let split_date = String.split ~on:'.'

let ints_of_string d =
  try match split_date d with
      [d;m;y] -> [Int.of_string y; Int.of_string m; Int.of_string d]
    |  _      -> failwith "Bad date format"
  with Failure _ -> failwith "Bad date format"

let ints_of_dpat d =
  let int_of_stringpat = function "_" -> 0 | s -> Int.of_string s in
  try match split_date d with
      [d;m;y] -> [ int_of_stringpat y ; int_of_stringpat m ; int_of_stringpat d ]
    | _ -> failwith "Bad date format"
  with Failure _ -> failwith "Bad date pattern"

(*  Given a relation r on integers, we now code the test function.
 *
 *  It simply consists of implementing the lexicographic order,
 *  taking into account the particular case of 0: *)

let rec eq_dtst d1 d2 =
  match d1 , d2 with
    [] , [] -> true
  | (0::d1) , (_::d2) -> eq_dtst d1 d2
  | (n1::d1) , (n2::d2) -> n1 = n2 && eq_dtst d1 d2
  | _ , _ -> failwith "Bad date pattern or format"

let rec cmp_dtst r d1 d2 =
  match d1 , d2 with
  | [] , [] -> true
  | (0::d1) , (_::d2) -> cmp_dtst r d1 d2
  | (n1::d1) , (n2::d2) -> (r n1 n2) || (n1 = n2 && cmp_dtst r d1 d2)
  | _ , _ -> failwith "Bad date pattern or format"

(* We finally define the generic function tst_dfield
 * which takes as arguments a relation r, a database db, a pattern dp, a field name nm, and a card dc.
 *
 * This function checks that the pattern and the field from the card satisfy the relation. *)
let tst_dfield r db dp nm dc =
  r (ints_of_dpat dp) (ints_of_string (field db nm dc))

(* we now apply it to three relations.  *)

let eq_dfield = tst_dfield eq_dtst
let after_dfield = tst_dfield (cmp_dtst (<))
let before_dfield = tst_dfield (cmp_dtst (>))

(* Thus, we can consider a test as a function of type data_card -> bool.
 *  We want to obtain boolean combinations of the results of such functions applied to a given card.
 *  To this end, we implement the iterator:  *)

let fold_funs b c fs dc =
  List.fold_right fs ~f:(fun f -> c (f dc)) ~init:b

(* where b is the base value, the function c is the boolean operator,
 * fs is the list of test functions on a field, and dc is a card *)

(* We can obtain the conjunction and the disjunction of a list of tests with: *)
let and_fold fs = fold_funs true (&&) fs
let or_fold fs = fold_funs false (||) fs

(* negation of a test *)
let not_fun f dc = not (f dc)

(* using these combinators to define a selecion function for cards
 * whose data field is included in a given range *)
let date_interval db d1 d2 =
  and_fold [(after_dfield db d1 "Date") ; (before_dfield db d2 "Date")]
