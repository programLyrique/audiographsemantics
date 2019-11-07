(** Executable semantics of an audio graph*)
open Batteries

(** Buffers *)
type samplebuffer = float array
type latency = float
type period = float
type buffer = latency * period * samplebuffer
