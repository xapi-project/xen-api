(* Idl_test_common generates these for full RPCs, not for individual types,
   however we want to check deserialisability of a record *)

open Xapi_metastore

let dir = Fpath.v "test_metastore_random_configs"

let () =
  let digest, tests = Config.gen_test () in
  let digest_hex = Digest.to_hex digest in
  Format.printf "Generating %d tests in %a/%s_%%d.json@." (List.length tests)
    Fpath.pp dir digest_hex ;
  tests
  |> List.iteri @@ fun i cfg ->
     (* when we change the data structure avoid accidentally overwriting old
        tests: use the schema hash as part of the filename.
        This ensures that we keep testing that old versions of the configuration
        can be loaded without errors
     *)
     let path = Fpath.(dir / Printf.sprintf "%s_%d.json" digest_hex i) in
     cfg |> Config.serialize |> Serialization.string_to_file_exn path
