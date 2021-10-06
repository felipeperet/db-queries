open Base

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
   let list_index =  (make_enum 0 ((List.length list_names) - 1)) in
   let assoc_index_name = (List.zip_exn list_names list_index)  in
   function name -> List.Assoc.find_exn ~equal:String.equal assoc_index_name name

(* function that reads a file of the given format *)
let read_base filename =
   let channel = Stdio.In_channel.create filename in
   let split_line = String.split ~on:':' in
   let list_names = String.split ~on:'|' (Stdio.In_channel.input_line_exn channel) in
   let rec read_file () =
     try
       let data = Array.of_list (split_line (Stdio.In_channel.input_line_exn channel)) in
         data :: (read_file ())
     with End_of_file ->  Stdio.In_channel.close channel ; []
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
let eq_sfield db s n dc = ((phys_equal) s (field db n dc))
let nonempty_sfield db n dc = not (phys_equal "" (field db n dc))
