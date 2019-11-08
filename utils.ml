(** Useful functions *)
open Batteries

(** Extend a list with less than n elements to n elements with a default value *)
let extend l n e = 
  let len = List.length l in 
    l @ (if List.length l < n then  (List.make (n - len ) e )
        else [] )