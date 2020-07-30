let version =
  (* returns n/a unless you add a version to dune-project *)
  match Build_info.V1.version () with
  | None -> "n/a"
  | Some v -> Build_info.V1.Version.to_string v
