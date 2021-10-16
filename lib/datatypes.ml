open Base

(* DATA FORMAT *)

type data_card = string array
type data_base = { card_index : string -> int ;
                   data : data_card list }

(* access to the field of a card *)
let field base name =
  let i = base.card_index name in fun (card : data_card) -> card.(i)

