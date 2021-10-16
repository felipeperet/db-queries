open Lib.Datatypes
open Lib.Formatting

let base_ex =
   { data = [ [|"Sasdelli"; "Felipe"|] ; [|"Ribeiro"; "Rodrigo"|] ]  ;
     card_index = function "Lastname" -> 0 | "Firstname" -> 1
                         | _ -> raise Not_found  }

let print_names () =
  List.iter print_string (List.map (format_line base_ex []) base_ex.data)

let () = print_names ()
