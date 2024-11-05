open OUnit2
open Problem_9

let expected_product = 31875000

let test_find_triplet name find_triplet _ =
  assert_equal (find_triplet ()) expected_product ~msg:name

let suite =
  "suite"
  >::: [
         "test_find_triplet_recursive"
         >:: test_find_triplet "test_find_triplet_recursive"
               find_triplet_recursive;
         "test_find_triplet_module"
         >:: test_find_triplet "test_find_triplet_module" find_triplet_module;
         "test_find_triplet_map"
         >:: test_find_triplet "test_find_triplet_map" find_triplet_map;
         "test_find_triplet_cycle"
         >:: test_find_triplet "test_find_triplet_cycle" find_triplet_cycle;
         "test_find_triplet_seq"
         >:: test_find_triplet "test_find_triplet_seq" find_triplet_seq;
       ]
