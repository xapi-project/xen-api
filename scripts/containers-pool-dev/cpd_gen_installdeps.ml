open Containergen
open Bos

let lockfile = OS.Arg.(opt ["lockfile"] ~absent:None @@ some path |> Option.get)
let lockfileid= OS.Arg.(opt ["lockfileid"] ~absent:None @@ some string |> Option.get)
let gitdescribe = OS.Arg.(opt ["gitdescribe"] ~absent:None @@ some string |> Option.get)

let repository = OS.Arg.(opt ["repository"] ~absent:"git+https://github.com/edwintorok/xs-opam" string)

let cwd = OS.Dir.current () |> Rresult.R.error_msg_to_invalid_arg

let tools =
  let alias = "duniverse" in
  alias, Dockerfile.(
    Opam.opam_from ~alias () @@@
    [ Command.run [Opam.install ["opam-monorepo"]]
    ; Command.run [Opam.monorepo_pull ~uniqueid:lockfileid ~lockfile]
    ]
  )

let installed_sys_deps ~sys_packages =
  Command.run
  [ (* command that adds extra repositories first in a separate command *)
    Yum.install ["epel-release"; "centos-release-xen"]
  ; Yum.install sys_packages
  ]

let install_opam_packages =
  Command.run [ Command.v (* TODO: commit hash *)
                Cmd.(
                  v "opam"
                  % "repository"
                  % "set-url"
                  % "default"
                  % repository
                )
        ; Opam.install_deps ~uniqueid:lockfileid ~lockfile
  ]

let build_xapi =
  (* for now release:true is required due to too many warnings *)
  Command.run @@ Dune.build ~release:true ~uniqueid:gitdescribe
    ~source:Fpath.(v ".") ["xapi.install"; "xe.install"; "message-switch-unix.install"; "forkexecd.install"]

let () =
  (* we read the arg from a file in dune, so get a single arg with spaces *)
  let split = String.split_on_char ' ' in
  let sys_packages = OS.Arg.(parse ~pos:string ()) |> List.concat_map split in
  Dockerfile.(
    Opam.opam_from ()
    @@@ [
          Command.run [Opam.install ["opam-monorepo"]]
        ; Command.run
            [
              Yum.install ["epel-release"; "centos-release-xen"]
            ; Yum.install packages
            ]
          (* TODO: git hash based *)
        ; Command.run
            [
              Command.v Cmd.(v "echo" % "TODO: commit hash 2")
            ; Command.v
                Cmd.(
                  v "opam"
                  % "repository"
                  % "set-url"
                  % "default"
                  % "git+https://github.com/edwintorok/xs-opam"
                )
            ]
        ; Command.run [Command.v Cmd.(v "mkdir" % "-p" % "workspace")]
          (* one layer for installing OS packages *)
        ; Dockerfile.copy ~chown:"1000:1000" ~src:["xapi.opam.locked"]
            ~dst:Fpath.(workspace / "xapi.opam.locked" |> to_string)
            ()
        ; Command.run (Opam.monorepo_pull Fpath.(home // workspace))
        ; Command.run
            [
              Opam.install ~ignore_pin_depends:true ~deps_only:true ~locked:true
                [Fpath.(workspace / "xapi.opam.locked" |> to_string)]
            ]
        ; Command.run [Command.v Cmd.(v "opam" % "remove" % "sexplib")]
        ; Command.run
            (Command.v Cmd.(v "touch" % "workspace/dune-workspace")
            :: Command.v
                 Cmd.(
                   v "ln"
                   % "-s"
                   % "xapi/dune-project"
                   % "workspace/dune-project"
                 )
            :: Dune.build ~release:true
                 ~source:Fpath.(v ".")
                 ~target:Fpath.(workspace / "xapi")
                 ()
            )
        ]
  )
  |> Generate.stdout
