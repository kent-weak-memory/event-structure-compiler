open OUnit2
module R = Relation

let test_symmetric1 test_ctxt = assert_equal false (R.is_symmetric [(1, 2); (2, 3); (3, 4)])
let test_symmetric2 test_ctxt = assert_equal true (R.is_symmetric [(1, 2); (2, 1); (3, 4); (4, 3)])
let test_symmetric3 test_ctxt = assert_equal true (R.is_symmetric [])
let test_symmetric4 test_ctxt = assert_equal true (R.is_symmetric [(1, 1)])

let test_asymmetric1 test_ctxt = assert_equal true (R.is_antisymmetric [(1, 2); (2, 3); (3, 4)])
let test_asymmetric2 test_ctxt = assert_equal false (R.is_antisymmetric [(1, 2); (2, 1); (3, 4); (4, 3)])
let test_asymmetric3 test_ctxt = assert_equal false (R.is_antisymmetric [])
let test_asymmetric4 test_ctxt = assert_equal false (R.is_antisymmetric [(1, 1)])

let test_reflexive1 test_ctxt = assert_equal true (R.is_reflexive [(1,1)] [1])
let test_reflexive2 test_ctxt = assert_equal false (R.is_reflexive [(1,1)] [1; 2])
let test_reflexive3 test_ctxt = assert_equal false (R.is_reflexive [(1,1); (2,1)] [1; 2])
let test_reflexive4 test_ctxt = assert_equal true (R.is_reflexive [(1,1); (2,2)] [1; 2])
let test_reflexive5 test_ctxt = assert_equal true (R.is_reflexive [(1,1); (2,2); (1,2)] [1; 2])

let test_edge_in_relation1 text_ctxt = assert_equal true (R.edge_in_relation (1,1) [(1,1)])
let test_edge_in_relation2 text_ctxt = assert_equal false (R.edge_in_relation (1,2) [(1,1)])
let test_edge_in_relation3 text_ctxt = assert_equal true (R.edge_in_relation (1,1) [(1,1); (1,2); (2,3)])
                                              

let suite =
"suite">:::
  ["is_symmetric antisymmetric relation">:: test_symmetric1;
   "is_symmetric symmetric relation">:: test_symmetric2;
   "is_symmetric empty relation">:: test_symmetric3;
   "is_symmetric singleton symmetric relation">:: test_symmetric4;

   "is_asymmetric antisymmetric relation">:: test_asymmetric1;
   "is_asymmetric symmetric relation">:: test_asymmetric2;
   "is_asymmetric empty relation">:: test_asymmetric3;
   "is_asymmetric singleton symmetric relation">:: test_asymmetric4;

   "is_reflexive reflexive singleton">:: test_reflexive1;
   "is_reflexive irreflexive singleton">:: test_reflexive2;
   "is_reflexive irreflexive">:: test_reflexive3;
   "is_reflexive reflexive pair">:: test_reflexive4;
   "is_reflexive reflexive pair +1">:: test_reflexive5;

   "edge_in_relation singleton true">:: test_edge_in_relation1;
   "edge_in_relation singleton false">:: test_edge_in_relation2;
   "edge_in_relation">:: test_edge_in_relation3]

let () =
run_test_tt_main suite
;;
