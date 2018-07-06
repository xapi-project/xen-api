open Base
open Stdio
module C = Configurator

let write_sexp fn sexp =
  Out_channel.write_all fn ~data:(Sexp.to_string sexp)

let () =
  C.main ~name:"mirage-clock-unix" (fun c ->
    let ccflags =
      match C.ocaml_config_var c "system" with
      | Some "linux" -> ["-lrt"]
      | _ -> [] in
    write_sexp "cclib.sexp" (Sexp.List (List.map ~f:(fun x -> Sexp.Atom x) ccflags));
    Out_channel.write_all "cclib" ~data:(String.concat ccflags  ~sep:" ")
  )
