(** Executable semantics of an audio graph*)
open Batteries

(** Buffers *)
module Buffer = struct
  (* Type definitions (our domains) *)
  type samplebuffer = float array
  type latency = float
  type period = float
  type buffer = latency * period * samplebuffer

  let latency (lat, _, _)  = lat
  let period (_, p, _) = p
  let samplebuffer (_,_, spbuf) = spbuf

  let length buffer = Array.length (samplebuffer buffer)
  let (@+) (lat, p, spbuf) latency = (lat + latency, p, spbuf)
end 

(** *)