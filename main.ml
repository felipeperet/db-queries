open Base
open Stdio

exception Not_found

(* these are some experiments on database queries *)

type data_card = string array
type data_base = { card_index : string -> int ;
                   data : data_card list }

(* access to the field of a card *)
let field base name =
  let i = base.card_index name in fun (card : data_card) -> card.(i)

(* small example *)
let base_ex =
   { data = [ [|"Chailloux"; "Emmanuel"|] ; [|"Manoury"; "Pascal"|] ]  ;
     card_index = function "lastName" -> 0 | "firstName" -> 1
                            | _ -> raise Not_found }

(* getting the lastName field for all cards of the database *)
let last_name = List.map ~f:(field base_ex "lastName") base_ex.data

(* from a list of strings, associates each string with an index
 * corresponding to its position in the list *)
let mk_index list_names =
   let rec make_enum a b = if a > b then [] else a::(make_enum (a+1) b) in
   let list_index = (make_enum 0 ((List.length list_names) - 1)) in
   let assoc_index_name = (List.zip_exn list_names list_index)  in
   function name -> List.Assoc.find_exn ~equal:String.equal assoc_index_name name

(* function that reads a file of the given format *)
let read_base filename =
   let channel = In_channel.create filename in
   let split_line = String.split ~on:':' in
   let list_names = String.split ~on:'|' (In_channel.input_line_exn channel) in
   let rec read_file () =
     try
       let data = Array.of_list (split_line (In_channel.input_line_exn channel)) in
         data :: (read_file ())
     with End_of_file -> In_channel.close channel ; []
   in
   { card_index = mk_index list_names ; data = read_file () }

(* opening the database *)
let base_ex = read_base "association.dat"

(* The goal of database processing is to obtain a state of the database.
 * Building such a state may be decomposed into three steps:
 *
 * 1. selecting, according to some given criterion, a set of cards;
 * 2. processing each of the selected cards;
 * 3. processing all the data collected on the cards. *)

(* selection criteria *)

(* for string fields *)
let eq_sfield db s n dc = (String.equal s (field db n dc))
let nonempty_sfield db n dc = not (String.equal "" (field db n dc))

(* for float fields *)
let tst_ffield r v db n dc = r v (Float.of_string (field db n dc))
let eq_ffield = tst_ffield (Float.equal)
let lt_ffield = tst_ffield (Float.(<=))
let gt_ffield = tst_ffield (Float.(>=))

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

let rec app_dtst_cmp r d1 d2 =
  match d1 , d2 with
    [] , [] -> true
  | (0::d1) , (_::d2) -> app_dtst_cmp r d1 d2
  | (n1::d1) , (n2::d2) -> ((r n1 n2) || (n1 = n2)) && (app_dtst_cmp r d1 d2)
  | _ , _ -> failwith "Bad date pattern or format"

let rec app_dtst_eq r d1 d2 =
  match d1 , d2 with
    [] , [] -> true
  | (0::d1) , (_::d2) -> app_dtst_eq r d1 d2
  | (n1::d1) , (n2::d2) -> (r n1 n2) && (app_dtst_eq r d1 d2)
  | _ , _ -> failwith "Bad date pattern or format"

(* We finally define the generic function tst_dfield
 * which takes as arguments a relation r, a database db, a pattern dp, a field name nm, and a card dc.
 *
 * This function checks that the pattern and the field from the card satisfy the relation. *)
let tst_dfield r db dp nm dc =
  r (ints_of_dpat dp) (ints_of_string (field db nm dc))

(* we now apply it to three relations.  *)
let eq_dfield = tst_dfield (app_dtst_eq (=))
let le_dfield = tst_dfield (app_dtst_cmp (<=))
let ge_dfield = tst_dfield (app_dtst_cmp (>=))

(* the test "is before July 1998" is written *)
let before_july1998 = ge_dfield base_ex "_.07.1998" "Date"

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
  and_fold [(le_dfield db d1 "Date") ; (ge_dfield db d2 "Date")]

(* some functions for testing *)

let dates = List.map ~f:(field base_ex "Date") base_ex.data

let before_july1998_t = List.map ~f:before_july1998 base_ex.data
let my_birthday_t = List.map ~f:(eq_dfield base_ex "03.03.1997" "Date") base_ex.data
let interval_t = List.map ~f:(date_interval base_ex "23.11.1996" "02.03.1999") base_ex.data
