open Containergen
open Bos

let source = OS.Arg.(opt ["source"] ~absent:None @@ some path |> Option.get)

let cwd = OS.Dir.current () |> Rresult.R.error_msg_to_invalid_arg

let () =
  (* TODO: include lockfile digest in name *)
  let workspace = Fpath.(v "workspace") in
  (* we read the arg from a file in dune, so get a single arg with spaces *)
  let split = String.split_on_char ' ' in
  let packages = OS.Arg.(parse ~pos:string ()) |> List.concat_map split in
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
            [ Command.v Cmd.(v "echo" % "TODO: commit hash ")
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
            ::
            (Command.v Cmd.(v "ln" % "-s" % "xapi/dune-project" % "workspace/dune-project"))
            :: Dune.build ~release:true ~watch:true
                 ~source:Fpath.(v ".")
                 ~target:Fpath.(workspace / "xapi")
                 ()
            )
        ]
  )
  |> Generate.stdout
