open Cmdliner

module Impl = struct
  let stream_decode output =
    Qcow_stream.stream_decode Unix.stdin output ;
    `Ok ()
end

module Cli = struct
  let output default =
    let doc = Printf.sprintf "Path to the output file." in
    Arg.(value & pos 0 string default & info [] ~doc)

  let stream_decode_cmd =
    let doc = "decode qcow2 formatted data from stdin and write a raw image" in
    let man =
      [
        `S "DESCRIPTION"
      ; `P "Decode qcow2 formatted data from stdin and write to a raw file."
      ]
    in
    Cmd.v
      (Cmd.info "stream_decode" ~doc ~man)
      Term.(ret (const Impl.stream_decode $ output "test.raw"))

  let cmds = [stream_decode_cmd]
end

let info =
  let doc = "minimal CLI for qcow-stream" in
  Cmd.info "qcow-stream-tool" ~version:"1.0.0" ~doc

let () =
  let cmd = Cmd.group info Cli.cmds in
  exit (Cmd.eval cmd)
