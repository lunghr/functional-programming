open OUnit2

let () =
  let suite =
    "general test" >::: [ Test_problem_9.suite; Test_problem_21.suite ]
  in
  run_test_tt_main suite
