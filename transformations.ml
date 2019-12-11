(** Stream operators *)

(* We will functorize that later... *)

open Audiostream

module type STREAMOPERATOR = sig 
  type operator = Buffer.buffer -> Buffer.buffer
  type toperator = Buffer.buffer list -> Buffer.buffer list
  type poperator = Buffer.buffer ->  Buffer.buffer -> Buffer.buffer list
  val map : AudioStream.stream -> operator -> AudioStream.stream

  (** transforms a pair of streams into a list of streams *)
  val map2 : AudioStream.stream * AudioStream.stream  -> poperator -> AudioStream.stream list
end

module StreamOperator : STREAMOPERATOR = struct
  type operator = Buffer.buffer -> Buffer.buffer
  type toperator = Buffer.buffer list -> Buffer.buffer list
  type poperator = Buffer.buffer ->  Buffer.buffer -> Buffer.buffer list



  let map s op = AudioStream.make_bufferstream (AudioStream.dom s) (List.map op (AudioStream.buffers s))
  let map2 streams op = 
    let s1,s2 = streams in 
    let dom1 = AudioStream.dom s1 in 
    let dom2 = AudioStream.dom s2 in 
    (* It operates similarly to a merge operation between two lists, but keeping only the common timestamps. Return a domain and list of buffers. *)
    let rec aux d1 d2 = 
      if not (AudioStream.is_empty s1) && not (AudioStream.is_empty s2) then
        begin
          let t1 = AudioStream.first d1 in 
          let t2 = AudioStream.first d2 in 
          if t1 = t2 then (t1, op ((AudioStream.streamfunc s1) t1) ((AudioStream.streamfunc s2) t2))::
            (aux (AudioStream.succ s1 (AudioStream.next s1 t1)) (AudioStream.succ s2 (AudioStream.next s2 t1)))
          else if t1 < t2 then aux (AudioStream.succ s1 (AudioStream.next s1 t1))  d2 
          else aux d1 (AudioStream.succ s2 (AudioStream.next s2 t1))
        end
      else (* There cannot be matching timestamps so we just stop iterating *)
        ([], [])
      in 
      (* Get a list of list of buffers. We need to transform it into a list of streams *)
      let domain, values = aux dom1 dom2 in 
      let values = Utils.reshape values in 
      List.map (AudioStream.make_bufferstream domain) values


end