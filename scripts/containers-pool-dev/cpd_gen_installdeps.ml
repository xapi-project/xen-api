open Containergen
open Bos

let source = OS.Arg.(opt ["source"] ~absent:None @@ some path |> Option.get)

let () =
  (* TODO: include lockfile digest in name *)
  let dir = Fpath.(v "xapi" / "") in
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
            [
              Command.v
                Cmd.(
                  v "opam"
                  % "repository"
                  % "set-url"
                  % "default"
                  % "git+https://github.com/edwintorok/xs-opam"
                )
            ]
          (* one layer for installing OS packages *)
        ; Dockerfile.copy ~chown:"1000:1000" ~src:["xapi.opam.locked"]
            ~dst:Fpath.(dir / "" |> to_string)
            ()
        ; Command.run (Opam.monorepo_pull Fpath.(home // dir))
        ; Command.run
            [
              Opam.install ~ignore_pin_depends:true ~deps_only:true ~locked:true
                [Fpath.(dir / "xapi.opam.locked" |> to_string)]
            ]
        ; Command.run [Command.v Cmd.(v "mkdir" % "-p" % "workspace")]
        ; Command.run
          [ Command.v Cmd.(v "touch" % "workspace/dune-workspace")
          ; Dune.build ~source ~target:Fpath.(home / "workspace" / "xapi")]
        ]
  )
  |> Generate.stdout
