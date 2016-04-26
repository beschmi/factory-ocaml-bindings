open Poly

val div : ipoly -> ipoly -> ipoly

val gcd : ipoly -> ipoly -> ipoly

val gcd_div : ipoly -> ipoly -> ipoly * ipoly * ipoly

val factor : ipoly -> (ipoly * int) list

val reduce : ipoly -> ipoly -> ipoly

val reduce_zero : ipoly -> ipoly -> bool

val ipoly_to_cpoly :
  Poly.IP.t -> int * int * Signed.long Ctypes_static.ptr Ctypes.ptr * Signed.long Ctypes.ptr

val cpoly_to_ipoly :
    int * int *
           ((Signed.Long.t, [ `C ]) Ctypes.pointer, [ `C ]) Ctypes.pointer *
           (Signed.Long.t, [ `C ]) Ctypes.pointer -> Poly.IP.t
