let () =
  print_endline "#include \"../lib/factory_wrapper.h\"";
  Cstubs.Types.write_c Format.std_formatter (module Ffi_bindings.Types)
