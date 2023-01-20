open Containergen
open Bos

let cwd = OS.Dir.current () |> Rresult.R.error_msg_to_invalid_arg

let duniverse ~lockfile =
  let alias = "duniverse" in
  let uniqueid = Digest.to_hex @@ Digest.file @@ Fpath.to_string lockfile in
  alias, Dockerfile.(
    Opam.opam_from ~alias () @@@
    [ Command.run [Opam.install ["opam-monorepo"]]
    ; Command.run [Command.v Cmd.(v "mkdir" % "-p" % p Dune.workspace)]
    ; Command.run [Command.v Cmd.(v "ls" % "-ld" % "/tmp")]
    ; Command.run [Opam.monorepo_pull ~uniqueid ~lockfile]
    ]
  )

let install_sys_deps ~sys_packages =
  Command.run
  [ (* command that adds extra repositories first in a separate command *)
    Yum.install ["epel-release"; "centos-release-xen"]
  ; Yum.install sys_packages
  ]

let install_opam_packages ~repository ~lockfile ~uniqueid =
  Command.run [ Command.v
                Cmd.(
                  v "opam"
                  % "repository"
                  % "set-url"
                  % "default"
                  % repository
                )
        ; Opam.install_deps ~lockfile ~uniqueid
  ]

let build_xapi ~uniqueid =
  (* for now release:true is required due to too many warnings *)
  Command.run @@ Dune.build ~release:true ~uniqueid
    ~source:Fpath.(v ".") ["xapi.install"; "xe.install"; "message-switch-unix.install"; "forkexec.install"]

let lockfile = OS.Arg.(opt ["lockfile"] ~absent:None @@ some path |> Option.get)
let opam_deps_id= OS.Arg.(opt ["opam-deps-id"] ~absent:None @@ some string |> Option.get)
let gitdescribe = OS.Arg.(opt ["gitdescribe"] ~absent:None @@ some string |> Option.get)

let repository = OS.Arg.(opt ["repository"] ~absent:None @@ some string |> Option.get)

let () =
  (* we read the arg from a file in dune, so get a single arg with spaces *)
  let split = String.split_on_char ' ' in
  let sys_packages = OS.Arg.(parse ~pos:string ()) |> List.concat_map split in
  let duniverse_stage, duniverse = duniverse ~lockfile in
  let duniverse_path = Fpath.(Dune.workspace / "duniverse" |> to_string) in
  Dockerfile.(
    duniverse
    @@@ [ Opam.opam_from ()
        ; install_sys_deps ~sys_packages
        ; Command.run [Command.v Cmd.(v "mkdir" % "-p" % p Dune.workspace % p
        Fpath.(home / ".cache" / "dune" / "db" / "files"))]
        ; install_opam_packages ~repository ~lockfile ~uniqueid:opam_deps_id
        ; Dockerfile.copy ~from:duniverse_stage ~src:[duniverse_path] ~dst:duniverse_path ()
        ; build_xapi ~uniqueid:gitdescribe
        ]
  )
  |> Generate.stdout
