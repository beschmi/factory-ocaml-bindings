open OUnit
open Lean

module Fac = Factory
module F  = Format

let t_XXX =
  "XXX" >:: fun () ->
    assert_bool "a1" true
    
let _ =
  let suite = "factory" >::: [
        t_XXX
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()
