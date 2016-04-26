open OUnit
open Factory
open Poly

let test_gcd_1 () =
  "test_gcd1" >:: fun () ->
  let open IP in
  let v1 = var 1 in
  let v2 = var 2 in
  let v3 = var 3 in
  let p1 = (v1 *@ v1 -@ (from_int 4)) *@ (v3 +@ one) in
  let p2 = (v1  +@ (from_int 2)) *@ v2 in
  let p3 = gcd p1 p2 in
  assert_bool "t1" (equal p3 (v1 +@ (from_int 2)))

let test_gcd_div_1 () =
  "test_gcd_div_1" >:: fun () ->
  let open IP in
  let v1 = var 1 in
  let v2 = var 2 in
  let v3 = var 3 in
  let p1 = (v1 *@ v1 -@ (from_int 4)) *@ (v3 +@ one) in
  let p2 = (v1  +@ (from_int 2)) *@ v2 in
  let p3 = gcd p1 p2 in
  let (p3',p4,p5) = gcd_div p1 p2 in
  assert_bool "t2" (equal p3 p3' && equal (div p1 p3) p4 && equal (div p2 p3) p5)

let test_gcd_2 () =
  "test_gcd_2" >:: fun () ->
  let open IP in
  let p1 = (from_int 4) in
  let p2 = (from_int 6) in
  let p3 = gcd p1 p2 in
  assert_bool "t1" (equal p3 (from_int 2))

let test_conversion () =
  "test_conversion" >:: fun () ->
  let open IP in
  let v1 = var 1 in
  let v2 = var 2 in
  let v3 = var 3 in
  let p1 = v1 *@ v2 -@ (from_int 4) in
  let p2 = v3 +@ (from_int 7) in
  let p3 = v3 *@ v2 *@ v1 +@ (from_int 99) in
  let p4 = p1 *@ p1 +@ p3 in
  let p5 = IP.from_int 0 in
  let p6 = from_int 1 in
  let cp1 = ipoly_to_cpoly p1 in
  let cp2 = ipoly_to_cpoly p2 in
  let cp3 = ipoly_to_cpoly p3 in
  let cp4 = ipoly_to_cpoly p4 in
  let cp5 = ipoly_to_cpoly p5 in
  let cp6 = ipoly_to_cpoly p6 in
  let p1' = cpoly_to_ipoly cp1 in
  let p2' = cpoly_to_ipoly cp2 in
  let p3' = cpoly_to_ipoly cp3 in
  let p4' = cpoly_to_ipoly cp4 in
  let p5' = cpoly_to_ipoly cp5 in
  let p6' = cpoly_to_ipoly cp6 in
  assert_bool "t1"
     (equal p1' p1 && equal p2' p2 && equal p3' p3 &&
      equal p4' p4 && equal p5' p5 && equal p6' p6)

let test_reduce_div () =
  "test_reduce_div" >:: fun () ->
  let open IP in
  let v1 = var 1 in
  let v2 = var 2 in
  let v3 = var 3 in
  let u = v1 *@ v2 -@ (from_int 4) in
  let v = v3 +@ (from_int 7) in
  let q = v3 *@ v2 *@ v1 +@ (from_int 99) in
  let w = u *@ v +@ q in
  let r = reduce w u in
  let d = div w u in
  let w' = d *@ u +@ r in
  assert_bool "t1" (equal w' w)

let test_factor () =
  "test_factor" >:: fun () ->
  let open IP in
  let v1 = var 1 in
  let v2 = var 2 in
  let v3 = var 3 in
  let u = v1 *@ v2 -@ (from_int 4) in
  let v = v3 +@ (from_int 7) in
  let q = v3 *@ v2 *@ v1 +@ (from_int 99) in
  let w = u *@ v *@ q in
  let ps = factor w in
  let res = List.fold_right (fun (f,e) g -> ring_exp f e *@ g) ps one in
  assert_bool "t1" (equal res w)
 
let _ =
  let suite = "factory" >::: [
        test_gcd_1 ();
        test_gcd_div_1 ();
        test_gcd_2 ();
        test_conversion ();
        test_reduce_div ();
        test_factor ();
      ]
  in
  OUnit2.run_test_tt_main @@ ounit2_of_ounit1 suite;
  Gc.full_major ()
