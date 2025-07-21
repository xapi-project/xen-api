module Impl = struct
  let stream_decode output =
    Qcow_stream.stream_decode Unix.stdin output ;
    `Ok ()
end

module Cli = struct
  open Cmdliner

  let stream_decode_cmd =
    let doc = "decode qcow2 formatted data from stdin and write a raw image" in
    let man =
      [
        `S "DESCRIPTION"
      ; `P "Decode qcow2 formatted data from stdin and write to a raw file."
      ]
    in
    let output default =
      let doc = Printf.sprintf "Path to the output file." in
      Arg.(value & pos 0 string default & info [] ~doc)
    in
    Cmd.v
      (Cmd.info "stream_decode" ~doc ~man)
      Term.(ret (const Impl.stream_decode $ output "test.raw"))

  let main () = Cmd.eval stream_decode_cmd
end

let () = exit (Cli.main ())
