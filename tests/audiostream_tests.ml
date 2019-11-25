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

let nb_buffers = 10 and sample_period = 1. and buffer_size = 12
let sample_periodic_s  = AudioStream.make_sample_periodic nb_buffers sample_period buffer_size 0.5 

let stream_length test_ctxt = 
  assert_equal 10 (AudioStream.length sample_periodic_s)

let periodic_timestamp test_ctxt =
  assert_equal 0. (AudioStream.first sample_periodic_s);
  assert_equal ((float_of_int ((nb_buffers - 1) * buffer_size)) *. sample_period) (AudioStream.last sample_periodic_s);
  assert_equal ((float_of_int buffer_size) *. sample_period) (AudioStream.next sample_periodic_s (AudioStream.first sample_periodic_s))

let suite = "audiostream" >::: ["singleton_stream" >:: singleton_stream;
                                "length" >:: stream_length;
                                "first timestamp" >:: periodic_timestamp
                                ]