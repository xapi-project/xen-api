let () =
  print_endline "#include <pci/pci.h>";
  Cstubs.Types.write_c Format.std_formatter (module Ffi_bindings.Types)
