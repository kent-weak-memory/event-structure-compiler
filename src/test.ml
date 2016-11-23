open OUnit2
module R = Relation

let test_symmetric1 test_ctxt = assert_equal false (R.is_symmetric [(1, 2); (2, 3); (3, 4)])
let test_symmetric2 test_ctxt = assert_equal true (R.is_symmetric [(1, 2); (2, 1); (3, 4); (4, 3)])
let test_symmetric3 test_ctxt = assert_equal true (R.is_symmetric [])
let test_symmetric4 test_ctxt = assert_equal true (R.is_symmetric [(1, 1)])

let test_reflexive1 test_ctx = assert_equal true (R.is_reflexive [(1,1)] [1])
let test_reflexive2 test_ctx = assert_equal false (R.is_reflexive [(1,1)] [1; 2])
let test_reflexive3 test_ctx = assert_equal false (R.is_reflexive [(1,1); (2,1)] [1; 2])
let test_reflexive4 test_ctx = assert_equal true (R.is_reflexive [(1,1); (2,2)] [1; 2])
let test_reflexive5 test_ctx = assert_equal true (R.is_reflexive [(1,1); (2,2); (1,2)] [1; 2])


let suite =
"suite">:::
  ["is_symmetric antisymmetric relation">:: test_symmetric1;
   "is_symmetric symmetric relation">:: test_symmetric2;
   "is_symmetric empty relation">:: test_symmetric3;
   "is_symmetric singleton symmetric relation">:: test_symmetric4;

   "is_reflexive reflexive singleton">:: test_reflexive1;
   "is_reflexive irreflexive singleton">:: test_reflexive2;
   "is_reflexive irreflexive">:: test_reflexive3;
   "is_reflexive reflexive pair">:: test_reflexive4;
   "is_reflexive reflexive pair +1">:: test_reflexive5]

let () =
run_test_tt_main suite
;;
