(* * Bindings for Lean *)
open Ctypes

(* ** Types module *)

module Types (F: Cstubs.Types.TYPE) = struct
  open F
  (* Fill in constant definitions here *)
end

(* ** Bindings module *)

module Bindings (F : Cstubs.FOREIGN) = struct
  open F
  (* Fill in type and function definitions here *)
end
                                         
