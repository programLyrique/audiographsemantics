open Batteries

module type BUFFER = sig 
  type samplebuffer = private float array 
  type latency 
  type period 
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
  val dom : stream -> defdomain 
  val streamfunc : stream -> streamfunction
  val first : stream -> timestamp
  val tail : stream -> defdomain
  val last : stream -> timestamp
  val next : stream -> timestamp -> timestamp
  val prec : stream -> timestamp -> defdomain
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

  (** dom(s) of a stream s *)
  let dom s = fst s
  (** To get the actual stream function of a stream s *)
  let streamfunc s = snd s

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
  let prec s t = 
    List.take_while (fun t' -> t' <= t) (dom s)
  let succ s t =
    List.drop_while (fun t' -> t' < t) (dom s)
end