open OUnit2
open Problem_21

let test_find_amicable_pairs_recursive _ =
  assert_equal 31626 (find_amicable_pairs_recursive 10000);
  assert_equal 504 (find_amicable_pairs_recursive 1000);
  assert_equal 504 (find_amicable_pairs_recursive 300);
  assert_equal 0 (find_amicable_pairs_recursive 100)

let test_find_amicable_pairs_tail _ =
  assert_equal 31626 (find_amicable_pairs_tail 10000);
  assert_equal 504 (find_amicable_pairs_tail 1000);
  assert_equal 504 (find_amicable_pairs_tail 300);
  assert_equal 0 (find_amicable_pairs_tail 100)

let test_find_amicable_pairs_module _ =
  assert_equal 31626 (find_amicable_pairs_module 10000);
  assert_equal 504 (find_amicable_pairs_module 1000);
  assert_equal 504 (find_amicable_pairs_module 300);
  assert_equal 0 (find_amicable_pairs_module 100)

let test_find_amicable_pairs_map _ =
  assert_equal 31626 (find_amicable_pairs_map 10000);
  assert_equal 504 (find_amicable_pairs_map 1000);
  assert_equal 504 (find_amicable_pairs_map 300);
  assert_equal 0 (find_amicable_pairs_map 100)

let test_find_amicable_pairs_cycle _ =
  assert_equal 31626 (find_amicable_pairs_cycle 10000);
  assert_equal 504 (find_amicable_pairs_cycle 1000);
  assert_equal 504 (find_amicable_pairs_cycle 300);
  assert_equal 0 (find_amicable_pairs_cycle 100)

let test_find_amicable_pairs_seq _ =
  assert_equal 31626 (find_amicable_pairs_seq 10000);
  assert_equal 504 (find_amicable_pairs_seq 1000);
  assert_equal 504 (find_amicable_pairs_seq 300);
  assert_equal 0 (find_amicable_pairs_seq 100)

let suite =
  "suite"
  >::: [
         "test_find_amicable_pairs_recursive"
         >:: test_find_amicable_pairs_recursive;
         "test_find_amicable_pairs_tail" >:: test_find_amicable_pairs_tail;
         "test_find_amicable_pairs_module" >:: test_find_amicable_pairs_module;
         "test_find_amicable_pairs_map" >:: test_find_amicable_pairs_map;
         "test_find_amicable_pairs_cycle" >:: test_find_amicable_pairs_cycle;
         "test_find_amicable_pairs_seq" >:: test_find_amicable_pairs_seq;
       ]
