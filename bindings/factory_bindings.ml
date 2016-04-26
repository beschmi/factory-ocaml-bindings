(* * Bindings for Lean *)
open Ctypes

(* ** Types module *)

module Types (F: Cstubs.Types.TYPE) = struct
  open F
  
  (* placeholder in case we need constants later on *)
  let _int_max  = constant "INT_MAX" int
end

(* ** Bindings module *)

module Bindings (F : Cstubs.FOREIGN) = struct
  open F

(* *** Types *)
  let cexpvecs = ptr (ptr long)
  let ccoeffs  = ptr long

  type distr_poly_struct
  let distr_poly_struct : distr_poly_struct structure typ = structure "DistrPoly"
  let dp_maxvar  = field distr_poly_struct "maxvar" int
  let dp_nterms  = field distr_poly_struct "nterms" int
  let dp_expvecs = field distr_poly_struct "expvecs" cexpvecs
  let dp_coeffs  = field distr_poly_struct "coeffs" ccoeffs
  let () = seal distr_poly_struct
  let distr_poly = typedef distr_poly_struct "DistrPoly"

  type dpoly_list
  let dpoly_list : dpoly_list structure typ = structure "DPolyList"
  let dpl_head     = field dpoly_list "head" (ptr distr_poly)
  let dpl_head_aux = field dpoly_list "head_aux" int
  let dpl_tail     = field dpoly_list "tail" (ptr_opt dpoly_list)
  let () = seal dpoly_list

(* *** Function bindings
 * ------------------------------------------------------------------------ *)

  let c_print_cpoly =
    foreign "wrap_print" (int @-> int @-> cexpvecs @-> ccoeffs @-> returning void)

  let c_new_expvecs =
    foreign "wrap_new_expvecs" (int @-> int @-> returning cexpvecs)

  let c_free_expvecs =
    foreign "wrap_free_expvecs" (cexpvecs @-> int @-> returning void)

  let c_free_dpoly_list =
    foreign "free_DPolyList" (ptr dpoly_list @-> returning void)

  let c_new_coeffs = foreign "wrap_new_coeffs" (int @-> returning ccoeffs)

  let c_free_coeffs = foreign "wrap_free_coeffs" (ccoeffs @-> returning void)

  let c_gcd = foreign "wrap_gcd"
    (int @-> int @-> cexpvecs @-> ccoeffs @->
     int @-> int @-> cexpvecs @-> ccoeffs @->
     returning distr_poly)

  let c_reduce = foreign "wrap_reduce"
    (int @-> int @-> cexpvecs @-> ccoeffs @->
     int @-> int @-> cexpvecs @-> ccoeffs @->
     returning distr_poly)

  let c_reduce_zero = foreign "wrap_reduce_zero"
    (int @-> int @-> cexpvecs @-> ccoeffs @->
     int @-> int @-> cexpvecs @-> ccoeffs @->
     returning int)

  let c_div = foreign "wrap_div"
    (int @-> int @-> cexpvecs @-> ccoeffs @->
     int @-> int @-> cexpvecs @-> ccoeffs @->
     returning distr_poly)

  let c_gcd_div = foreign "wrap_gcd_div"
    (int @-> int @-> cexpvecs @-> ccoeffs @->
     int @-> int @-> cexpvecs @-> ccoeffs @->
     returning (ptr dpoly_list))

  let c_factor = foreign "wrap_factor"
    (int @-> int @-> cexpvecs @-> ccoeffs @->
     returning (ptr dpoly_list))

  (*
  let new_expvecs = foreign
    "wrap_new_expvecs" (int @-> int @-> returning (ptr (ptr long)))
  *)
end
