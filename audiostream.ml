open Batteries

module type BUFFER = sig 
  type samplebuffer =  float array 
  type latency = float
  type period = float
  type buffer = private latency * period * samplebuffer

  val latency : buffer -> latency
  val period : buffer -> period 
  val samplebuffer : buffer -> samplebuffer
  val length : buffer -> int 
  val (@+) : buffer -> latency -> buffer 
  val make : latency -> period -> samplebuffer -> buffer 
end 

(** Buffers *)
module Buffer : BUFFER = struct
  (* Type definitions (our domains) *)
  type samplebuffer = float array
  type latency = float
  type period = float
  type buffer = latency * period * samplebuffer

  let latency (lat, _, _)  = lat
  let period (_, p, _) = p
  let samplebuffer (_,_, spbuf) = spbuf

  let length buffer = Array.length (samplebuffer buffer)
  let (@+) ((lat, p, spbuf) : buffer) latency = (lat +. latency, p, spbuf)

  let make latency p spbuf  = (latency, p, spbuf) 
end 

(** Streams
We could write a functor to parametrize with different buffer definitions.
 *)

module type STREAM = sig 
  type timestamp = float
  type defdomain 
  type streamfunction = timestamp -> Buffer.buffer 
  type stream = private defdomain * streamfunction

  val make : defdomain -> (timestamp * Buffer.buffer) list -> stream
  val make_f : defdomain -> streamfunction -> stream
  val make_samplestream : defdomain -> float list -> stream
  val make_singleton : timestamp -> Buffer.buffer -> stream
  val emptystream : stream
  val is_empty : stream -> bool

  val dom : stream -> defdomain 
  val streamfunc : stream -> streamfunction
  val first : stream -> timestamp
  val tail : stream -> defdomain
  val last : stream -> timestamp
  val next : stream -> timestamp -> timestamp
  val length : stream -> int
  val prec : stream -> timestamp -> defdomain
  val concat : stream -> stream -> stream
  val at : stream -> int -> timestamp
  val substream : stream -> timestamp -> timestamp -> stream
end

module Stream : STREAM = struct 
  type timestamp = float
  (* Finite stream for now. We could use an Enum (infinite stream) in the Batteries lib to represent infinite streams *)
  type defdomain = timestamp list (* Could be an ordered set for better performance of search *)
  type streamfunction = timestamp -> Buffer.buffer
  type stream = defdomain * streamfunction

  (** To create actual stream, from a definition domain and an association list of values. 
  We could use a more efficient way to store the values, such as a hastable or a map.
   *)
  let make defdomain values_assoc  = 
    (defdomain , (function t -> List.assoc t values_assoc) )  
  
  (** Create from a domain and a function. *) 
  let make_f defdomain f = (defdomain, f)

  (** Make a stream of samples *)
  let make_samplestream defdomain  samples =
    let samplebuffers  = List.map (fun sample -> Buffer.make 0. 1. (Array.singleton sample)) samples in 
    (* Combine will raise if the two argument lists have different sizes *)
    make defdomain (List.combine defdomain samplebuffers )

  let make_singleton (timestamp : timestamp) (buffer : Buffer.buffer) =
    ([timestamp], function (t : timestamp)-> buffer)

  (** dom(s) of a stream s *)
  let dom s = fst s
  (** To get the actual stream function of a stream s. We encapsulate it 
    into a check that the given value is in the definition domain.
    However, it can be wrapped several times so maybe we should displace it into the make functions themselves. *)
  let streamfunc s = 
    function t -> assert (List.mem t (dom s)); (snd s) t
  
  let emptystream = ([], function t -> undefined "Empty stream")
  let is_empty s = List.is_empty (dom s)



  (* handling timestamps *)
  let first s = List.hd (dom s)
  let tail s = List.tl (dom s)
  let last s = List.last (dom s)
  let next s t = 
    let rec aux = function 
      | t'::l when t' > t ->  t'
      | t'::l -> aux l
      | [] -> failwith "No next timestamp"
    in 
    aux (dom s)
  let length s = List.length (dom s)
  let prec s t = 
    List.take_while (fun t' -> t' <= t) (dom s)
  let succ s t =
    List.drop_while (fun t' -> t' < t) (dom s)
  let concat s s' =
    if is_empty s then s' 
    else if is_empty s' then s 
    else 
      begin
        assert (last s < first s');
        ((dom s) @ (dom s'), function t -> (streamfunc (if List.mem t (dom s) then s else s')) t)
      end
   
  (* Seeing a stream as a sequence with indices *)
  let at s  = List.at (dom s) 

  let substream s t1 t2 =
    let new_defdomain = List.take_while (fun t' -> t' <= t2) (List.drop_while (fun t' -> t' >= t1) (dom s)) in 
    make_f new_defdomain (streamfunc s) 

end