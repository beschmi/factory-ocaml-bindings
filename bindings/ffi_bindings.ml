(* * Bindings for Lean *)
open Ctypes

(* ** Types module *)

module Types (F: Cstubs.Types.TYPE) = struct
  open F
  let null_  = constant "INT_MAX" int
  (* Fill in constant definitions here *)
end

(* ** Bindings module *)

module Bindings (F : Cstubs.FOREIGN) = struct
  open F
  let new_expvecs = foreign
    "wrap_new_expvecs" (int @-> int @-> returning (ptr (ptr long)))

  (* Fill in type and function definitions here *)
end
