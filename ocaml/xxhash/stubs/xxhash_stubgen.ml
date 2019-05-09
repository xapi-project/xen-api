let stubgen prefix c_headers bindings =
  let ml_out = open_out (prefix ^ "_generated.ml")
  and c_out = open_out (prefix ^ "_stubs.c") in
  let ml_fmt = Format.formatter_of_out_channel ml_out
  and c_fmt = Format.formatter_of_out_channel c_out in
  Format.fprintf c_fmt "%s@." c_headers;
  Cstubs.write_c c_fmt ~prefix bindings;
  Cstubs.write_ml ml_fmt ~prefix bindings;
  Format.pp_print_flush ml_fmt ();
  Format.pp_print_flush c_fmt ();
  close_out ml_out;
  close_out c_out

let () = stubgen "xxhash" "#include <xxhash.h>" (module Xxhash_bindings.C)
