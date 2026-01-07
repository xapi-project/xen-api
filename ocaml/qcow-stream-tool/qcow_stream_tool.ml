open Cmdliner

module Impl = struct
  let stream_decode output =
    Qcow_stream.stream_decode Unix.stdin output ;
    `Ok ()

  let read_headers qcow_path =
    let open Lwt.Syntax in
    let t =
      let* fd = Lwt_unix.openfile qcow_path [Unix.O_RDONLY] 0 in
      let* virtual_size, cluster_bits, _, data_cluster_map =
        Qcow_stream.start_stream_decode fd
      in
      let clusters = Qcow_types.Cluster.Map.bindings data_cluster_map in
      let clusters =
        List.map
          (fun (_, virt_address) ->
            let ( >> ) = Int64.shift_right_logical in
            let address =
              Int64.to_int (virt_address >> Int32.to_int cluster_bits)
            in
            `Int address
          )
          clusters
      in
      let json =
        `Assoc
          [
            ("virtual_size", `Int (Int64.to_int virtual_size))
          ; ("cluster_bits", `Int (Int32.to_int cluster_bits))
          ; ("data_clusters", `List clusters)
          ]
      in
      let json_string = Yojson.to_string json in
      let* () = Lwt_io.print json_string in
      let* () = Lwt_io.flush Lwt_io.stdout in
      Lwt.return_unit
    in
    Lwt_main.run t ; `Ok ()
end

module Cli = struct
  let output default =
    let doc = Printf.sprintf "Path to the output file." in
    Arg.(value & pos 0 string default & info [] ~doc)

  let input =
    let doc = Printf.sprintf "Path to the input file." in
    Arg.(required & pos 0 (some string) None & info [] ~doc)

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

  let read_headers_cmd =
    let doc =
      "Determine allocated clusters by parsing qcow2 file at the provided \
       path. Returns JSON like the following: {'virtual_size': X, \
       'cluster_bits': Y, 'data_clusters': [1,2,3]}"
    in
    let man = [`S "DESCRIPTION"; `P doc] in
    Cmd.v
      (Cmd.info "read_headers" ~doc ~man)
      Term.(ret (const Impl.read_headers $ input))

  let cmds = [stream_decode_cmd; read_headers_cmd]
end

let info =
  let doc = "minimal CLI for qcow-stream" in
  Cmd.info "qcow-stream-tool" ~version:"1.0.0" ~doc

let () =
  let cmd = Cmd.group info Cli.cmds in
  exit (Cmd.eval cmd)
