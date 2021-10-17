open Base
open Stdio

open Lib
open Lib.Datatypes

(* list of mail addresses of a given database *)
let mail_addresses db =
  let dcs = List.filter ~f:(Process.eq_sfield db "mail" "Pref") db.data in
  List.map ~f:(Format.line db ["Address"]) dcs

(* list of email addresses of a given database *)
let email_addresses db =
  let dcs = List.filter ~f:(Process.eq_sfield db "email" "Pref") db.data in
  List.map ~f:(Format.line db ["Email"]) dcs

(* computing the state of the received fees during an interval *)
let fees_state db d1 d2 =
  let dcs = List.filter ~f:(Process.date_interval db d1 d2) db.data in
  let ls  = List.map ~f:(Format.line db ["Date";"Amount"]) dcs in
  let t   = Compute.total db dcs in (ls , t)

(* main program *)
let () =
  let db = Read.database "../../../data/association.dat" in
  let finished = ref false in
   while not !finished do
    print_string " 1: List of mail addresses\n";
    print_string " 2: List of email addresses\n";
    print_string " 3: Received fees\n";
    print_string " 0: Exit\n";
    print_string "Your choice: ";
    match (Caml.read_int ()) with
      0 -> finished := true
    | 1 -> (List.iter ~f:print_string (mail_addresses db))
    | 2 -> (List.iter ~f:print_string (email_addresses db))
    | 3
      -> (print_string "Start date: "; let d1 = Caml.read_line () in
         print_string "End date: "  ; let d2 = Caml.read_line () in
         let ls, t = fees_state db d1 d2 in
         List.iter ~f:print_string ls;
         printf "Total: %f\n" t )
    | _ -> ()
  done;;
