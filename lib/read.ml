open Base
open Stdio

open Datatypes

(* Reading a database from a file *)

(* from a list of strings, associates each string with an index
 * corresponding to its position in the list *)
let mk_index list_names =
   let rec make_enum a b = if a > b then [] else a::(make_enum (a+1) b) in
   let list_index = (make_enum 0 ((List.length list_names) - 1)) in
   let assoc_index_name = (List.zip_exn list_names list_index)  in
   function name -> List.Assoc.find_exn ~equal:String.equal assoc_index_name name

(* function that reads a file of the given format *)
let database filename =
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

