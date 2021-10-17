open Base

open Lib
open Lib.Datatypes

(*  We expect two kinds of queries on our database:
 *
 *  A query returning two lists, the elements of the first containing the name of a member
 * followed by his mail address, the elements of the other containing the name of the member followed
 * by his email address, according to his preferences.
 *
 *  And another query returning the state of received fees for a given period of time.
 *  This state is composed of the list of last and first names, dates and amounts of the fees
 * as well as the total amount of the received fees.  *)

(* To create these lists, we first select the relevant cards according to the field "Pref",
 * then we use the formatting function format_line:  *)

let mail_addresses db =
  let dcs = List.filter ~f:(Process.eq_sfield db "mail" "Pref") db.data in
  List.map ~f:(Format.line db ["Mail"]) dcs

let email_addresses db =
  let dcs = List.filter ~f:(Process.eq_sfield db "email" "Pref") db.data in
  List.map ~f:(Format.line db ["Email"]) dcs

(* computing the state of the received fees *)
let fees_state db d1 d2 =
  let dcs = List.filter ~f:(Process.date_interval db d1 d2) db.data in
  let ls = List.map ~f:(Format.line db ["Date";"Amount"]) dcs in
  let t = Compute.total db dcs in (ls , t)
