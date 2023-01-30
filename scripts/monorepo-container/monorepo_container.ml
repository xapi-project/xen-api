open Bos_setup

let make_cmd str =
  Cmd.of_string str |> R.error_msg_to_invalid_arg

let opam_host = make_cmd "opam --cli=2.1"

let opam_container = make_cmd "opam-2.1 --cli=2.1"

let with_temp_switchdir f =
  OS.Dir.with_tmp "_opam_monorepo_container.%s" f () |> R.join

let step name f =
  Logs.info (fun m -> m "[%s]" name) ;
  f () |> R.reword_error_msg @@ fun _ -> R.msgf "Error in step [%s]" name

type t = {
    opam_depexts: string list
  ; opam_packages: string list
  ; all_depexts: string list
}

let out_string_lines cmd =
  cmd
  |> OS.Cmd.to_lines
  |> R.map @@ List.concat_map @@ String.cuts ~rev:false ~empty:false ~sep:" "

(* we run the query on the host which may be different from the container *)
let env =
  let add = String.Map.of_list
  [
     "OPAMVAR_arch", "x86_64"
     ; "OPAMVAR_os_family", "centos"
     ; "OPAMVAR_os_distribution", "centos"
     ; "OPAMVAR_os_version", "7"
  ]
  in
  OS.Env.current () |> R.failwith_error_msg
  |> String.Map.union (fun _ added _ -> Some added) add

let opam_provided_packages_of_lockfile ~repositories ~lockfile =
  with_temp_switchdir @@ fun switchdir () ->
  let repos =
    let join (k, v) = "_container_" ^ k ^ "=" ^ v in
    let some pair_lst =
      Cmd.(
        v "--repositories"
        % (pair_lst |> List.map join |> String.concat ~sep:",")
      )
    in
    Option.fold ~none:Cmd.empty ~some repositories
  in

  step "Creating temporary switch for querying packages" @@ fun () ->
  OS.Cmd.run
    Cmd.(
      opam_host % "switch" % "create" % "-q" %% repos % p switchdir % "--empty"
    )
  >>= fun () ->
  let opam_switch_cmd subcmd =
    Cmd.(opam_host % subcmd % ("--switch=" ^ p switchdir))
  in
  let pkg_name_of p = Fpath.(v "." // p |> to_string) in

  step
    "Find all opam provided package versions by fake installing in temporary \
     switch"
  @@ fun () ->
  OS.Cmd.run_out
    Cmd.(
      opam_switch_cmd "install"
      % "-q"
      % "-y"
      % "--fake"
      % "--ignore-pin-depends"
      % "--deps-only"
      % pkg_name_of lockfile
      % "--locked"
    )
  |> OS.Cmd.out_null
  |> OS.Cmd.success
  >>= fun () ->
  step "Listing opam provided packages and versions" @@ fun () ->
  OS.Cmd.run_out
    Cmd.(
      opam_switch_cmd "list"
      % "-q"
      % "--color=never"
      % "--columns=package"
      % "--short"
    )
  |> out_string_lines
  >>= fun opam_packages ->
  step "Querying depexts of opam provided packages" @@ fun () ->
  OS.Cmd.run_out ~env
    Cmd.(opam_switch_cmd "show" % "-f" % "depexts" %% of_list opam_packages)
  |> out_string_lines
  >>= fun opam_depexts ->
  step "Querying depexts of all locked packages" @@ fun () ->
  OS.Cmd.run_out ~env Cmd.(opam_switch_cmd "show" % "-f" % "depexts" % p lockfile)
  |> out_string_lines
  >>| fun all_depexts -> {opam_packages; opam_depexts; all_depexts}

let ocaml_version =
  Ocaml_version.(sys_version |> with_just_major_and_minor |> to_string)

let container_image_of_distro t =
  let distro = Dockerfile_opam.Distro.tag_of_distro t in
  Printf.sprintf "docker.io/ocaml/opam:%s-ocaml-%s" distro ocaml_version

let distro_tag =
  let distro_of_tag =
    OS.Arg.parser_of_kind_of_string ~kind:"distro tag"
      Dockerfile_opam.Distro.distro_of_tag
  in
  let tag_of_distro = Fmt.of_to_string Dockerfile_opam.Distro.tag_of_distro in
  OS.Arg.conv distro_of_tag tag_of_distro

(** [required_arg ?docv ?doc ?env names converter]
  is like [OS.Arg.opt] but returns an error message if the argument is absent
*)
let required_arg ?docv ?doc ?env names converter =
  let none () =
    let doc_fmt = Fmt.(option @@ fmt "Missing %s.@ ") in
    let names_fmt = Fmt.(list ~sep:(any "@ or ") @@ (any "--" ++ string)) in
    R.error_msgf "%a%a must be specified" doc_fmt doc names_fmt names
  in
  OS.Arg.(opt ?docv ?doc ?env names (some converter) ~absent:None)
  |> R.of_option ~none

let distro_result =
  required_arg ~docv:"DISTRO" ~doc:"dockerfile opam distro tag to use in FROM"
    ["distro"] distro_tag

let repositories =
  OS.Arg.(
    opt ~docv:"REPO=URL[,...]"
      ~doc:"opam repository suitable for switch create --repositories"
      ["repositories"]
      (some @@ list ~sep:"," @@ pair ~sep:"=" string string)
      ~absent:None
  )

let debug = OS.Arg.(flag ~doc:"enable debug log-level" ["debug"])

let lockfile_result =
  required_arg ~docv:"LOCKFILE" ~doc:"opam lockfile" ["lockfile"] OS.Arg.path

let build_targets =
  OS.Arg.(
    parse ~doc:"Generates a Containerfile from a monorepo lockfile" ~pos:string
      ()
  )

module Config = struct let container_uid, container_gid = (1000, 1000) end

let home = Fpath.(v "/home" / "opam")

module Stage = struct
  type t = {alias: string option; dockerfile: Dockerfile.t}

  let v ?alias ?tag ?platform from_image lst =
    {
      alias
    ; dockerfile=
        Dockerfile.(
          from ?alias ?tag ?platform from_image
          @@@ lst
        )
    }

  let to_dockerfile t = t.dockerfile

  (* TODO: doesn't quite work with podman, can't find dir on host from stage,
     apparently a buildah bug fixed very recently but not released yet
  *)
  let with_ro_mount ~from ~source ~target t =
    let mount = Dockerfile.Mount.bind ~from_source:(from.alias, Fpath.(home // source |> to_string)) target in
    Dockerfile.with_mounts [mount] [t]

  let copy_from ~from ~source ~target =
    Dockerfile.copy ~from:(Option.get from.alias)
         ~src:[Fpath.(home // source |> to_string)]
         ~dst:Fpath.(home // target |> to_string)
         ()
end

module Containerfile = struct
  let output stages =
    Dockerfile.(
      parser_directive (`Syntax "docker/dockerfile:1")
      @@@ List.map Stage.to_dockerfile stages
    )
    |> Dockerfile.string_of_t
    |> print_endline
end

let with_user_cache = Dockerfile_opam.with_user_cache
let with_build_cache ~id ~target t =
  Dockerfile.with_mounts [Dockerfile.Mount.cache ~uid:Config.container_uid ~gid:Config.container_gid ~id Fpath.(to_string target)] [t]

let with_writediscard_mount ~uniqueid:_ ~host_source ~target t =
  let mount =
    Dockerfile.Mount.bind ~from_source:(None, Fpath.to_string host_source)
    (* rw here means that writes are permitted, but do not take effect on the host *)
    (Fpath.(to_string target) ^ ",rw")
  in
  Dockerfile.with_mounts [mount]
    (* TODO: build arg doesn't actually take effect here?? *)
    [ t
    ]

let installer_of_distro distro =
  Dockerfile_opam.distro_install ~cache:true ~switch_user:true distro

let cmd_run cmd = Dockerfile.run "%s" Cmd.(to_string cmd)

let cmds lst =
  lst
  |> List.map cmd_run
  |> List.fold_left Dockerfile.( @@ ) Dockerfile.empty
  |> Dockerfile.crunch

let symlink ~link_source ~link_target =
  let link_source =
    Fpath.relativize ~root:Fpath.(parent link_target) link_source |> Option.get
  in
  Cmd.(v "ln" % "-sf" % p link_source % p link_target)

let link_target = Fpath.(v ".opam" / "download-cache")
let link_source = Fpath.(v ".cache" / "opam" / "download-cache")

let of_list = function
  | [] -> Dockerfile.empty
  | hd :: tl -> Dockerfile.(hd @@@ tl)

let opam_install ?(repositories = []) packages =
  with_user_cache
  @@
  let repo_cmds =
    match repositories with
    | [] ->
        []
    | repos ->
        Cmd.(opam_container % "repo" % "remove" % "default")
        :: (repos
           |> List.map @@ fun (name, url) ->
              Cmd.(opam_container % "repo" % "add" % name % url)
           )
  in
      [
        cmd_run Cmd.(v "rm" % p link_target % "-rf")
      ; cmd_run Cmd.(v "mkdir" % "-p" % p link_source)
      ; cmd_run (symlink ~link_source ~link_target)
      ; repo_cmds |> List.map cmd_run |> of_list
      ; cmd_run Cmd.(opam_container % "install" %% of_list packages)
      ]

let dune_workspace = Fpath.v "workspace"

let monorepo_pull ~lockfile =
  let lockfile_dst = Fpath.(dune_workspace // base lockfile) in
  let digest = Digest.file Fpath.(to_string lockfile) |> Digest.to_hex in
  (* could use COPY, but that is based on timestamps,
     which won't work well with caching, a digest is more reliable *)
  with_writediscard_mount ~uniqueid:digest ~host_source:lockfile ~target:lockfile_dst
  @@ with_user_cache
  @@ [ cmd_run Cmd.(v "mkdir" % "-p" % p link_source)
           ;  cmd_run Cmd.(
               opam_container
               % "monorepo"
               % "pull"
               % "-r"
               % p Fpath.(home // dune_workspace)
               % "-l"
               % p lockfile_dst
             )
         ]

let dune_build ?(release = false) ~argname ~source_build_id targets =
  let uniqueid = "${" ^ argname ^ "}" in
  let target = Fpath.(dune_workspace / "source_ro") in
  let prefix = Fpath.(home / "prefix") in
  with_writediscard_mount ~uniqueid ~host_source:Fpath.(v ".") ~target
  @@ with_build_cache ~id:source_build_id
       ~target:Fpath.(dune_workspace / "_build")
  @@ with_user_cache
         [ Dockerfile.run ": %s" uniqueid
         ;  cmd_run Cmd.(v "touch" % p Fpath.(dune_workspace / "dune-workspace"))
         ; cmd_run Cmd.(v "cd" % p target)
         (* FIXME: xapi specific *)
         ; cmd_run Cmd.(v "./configure" % "--prefix" % p prefix)
         ; cmd_run
             Cmd.(
               opam_container
               % "exec"
               % "--"
               % "dune"
               % "build"
               % "--root"
               % p Fpath.(home // dune_workspace)
               %% on release (v "--profile=release")
               %% of_list
                    (List.map
                       (fun p -> Fpath.(v "source_ro" / p |> to_string))
                       targets
                    )
             )
         ; cmd_run
             Cmd.(
               opam_container
               % "exec"
               % "--"
               % "dune"
               % "install"
               % "--prefix"
               % p Fpath.(home / "prefix")
               %% (targets
                  |> List.map Fpath.v
                  |> List.map Fpath.rem_ext
                  |> of_values Fpath.to_string
                  )
             )
        ; cmd_run Cmd.(v "sudo" % "make" % "-C" % "scripts" % "install" % "DESTDIR=/")
         ; cmd_run Cmd.(v "cp" % p Fpath.(v "run.sh") % p Fpath.(home / "prefix" / "run.sh"))
         ]

let duniverse_path = Fpath.(dune_workspace / "duniverse")

let dockerfile_duniverse_stage ~image ~lockfile =
  (* install monorepo into a container build stage of its own,
     so it doesn't interfere with installing deps of our repo.
     Run monorepo pull in that layer, and then copy it over.
  *)
  Stage.v ~alias:"duniverse" image
      Dockerfile.[
        (* TODO: hardlink doesn't quite yet work yet, EXDEV *)
        env [("DUNE_CACHE", "enabled"); ("DUNE_CACHE_STORAGE_MODE", "copy")]
      ; opam_install ["opam-monorepo"]
      ; run "mkdir -p %s" Fpath.(to_string dune_workspace)
      ; monorepo_pull ~lockfile
      ]


let main =
  if debug then Logs.set_level ~all:true (Some Logs.Debug) ;
  distro_result >>= fun distro ->
  lockfile_result >>= fun lockfile ->
  let installer =
    distro
    |> installer_of_distro
  in
  let image = container_image_of_distro distro in
  let duniverse_stage = dockerfile_duniverse_stage ~image ~lockfile in
  opam_provided_packages_of_lockfile ~repositories ~lockfile >>| fun t ->
  let source_build_id =
    Printf.sprintf "build:%s;%s" image @@ Fpath.to_string lockfile
  in
  let argname = "GITDESCRIBE" in
  let main_stage =
    (* layers ordered from least likely to change to most likely *)
    Stage.v image
      Dockerfile.
        [
          (* package that installs additional repos must be separate *)
          (* TODO: only for centos *)
          installer ["epel-release"; "centos-release-xen"]
        ; installer t.opam_depexts
          (* minimal system deps just for opam packages *)
        ; env [("DUNE_CACHE", "enabled"); ("DUNE_CACHE_STORAGE_MODE", "copy")]
        ; cmd_run Cmd.(v "mkdir" % "-p" % p dune_workspace)
          (* TODO: might need storage mode copy if we get EXDEV from the cache *)
        ; opam_install ?repositories t.opam_packages
          (* install opam provided packages, these change rarely *)
        ; installer t.all_depexts
          (* install depexts for duniverse+local packages, avoids recompiling opam packages if this changes *)
        ; installer ["etcd"; "/usr/sbin/ip"; "stunnel"; "strace"; "openssl"]
        ; Dockerfile.arg argname
        ; Stage.copy_from ~from:duniverse_stage ~source:duniverse_path
            ~target:duniverse_path
        ; Dockerfile.run "mkdir -p /home/opam/workspace/source_ro /home/opam/workspace/_build"
        ; dune_build ~argname ~release:true ~source_build_id build_targets
        ]

  in

  Containerfile.output [duniverse_stage; main_stage]

let () = R.failwith_error_msg main
