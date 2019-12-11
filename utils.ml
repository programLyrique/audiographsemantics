(** Useful functions *)
open Batteries

(** Extend a list with less than n elements to n elements with a default value *)
let extend l n e = 
  let len = List.length l in 
    l @ (if List.length l < n then  (List.make (n - len ) e )
        else [] )

(** Transforms a list of list into a list of list, exchanging the dimensions. *)
let reshape = function 
  | [] -> [] 
  | s::l -> 
    let nb_lists = List.length s in 
    let rec aux = function 
      | [] -> List.make nb_lists []
      | s::l -> let tailLists = aux l in 
        List.map2 (fun bigList elem -> elem::bigList) tailLists s
    in 
    aux (s::l)