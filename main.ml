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
     function name -> List.Assoc.find ~equal:String.equal assoc_index_name name
