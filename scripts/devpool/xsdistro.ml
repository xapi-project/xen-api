open Bos_setup

(** distros to generate containers for *)
let distros =
  [
    ("stable", `CentOS `V7)
    (* the currently supported distro of the XAPI project *)
  ; ("future", `Fedora `Latest)
    (* for future-proofing, the very latest RPM based *)
  ]

let () =
  distros
  |> List.iter @@ fun (name, t) ->
     t
     |> Dockerfile_opam.Distro.tag_of_distro
     |> OS.File.write Fpath.(v name |> add_ext ".distro")
     |> R.failwith_error_msg
