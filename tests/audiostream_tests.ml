open OUnit2
open Batteries 
open Audiostream 


let singleton_stream test_ctxt =
  let timestamp = 2.4 in 
  let samplebuf = Array.singleton 4. in 
  let buf = Buffer.make 0. 1. samplebuf in 
  assert_equal 0.  (Buffer.latency buf);
  assert_equal 1. (Buffer.period buf);
  assert_equal samplebuf (Buffer.samplebuffer buf)

let sample_periodic_s  = Stream.make_sample_periodic 10 1. 12 0.5



let suite = "audiostream" >::: ["singleton_stream" >:: singleton_stream]