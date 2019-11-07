open OUnit2

let tests = "tests" >::: [Audiostream_tests.suite]


let () = 
  run_test_tt_main tests