open Bos_setup

let opam_host = Cmd.(v "opam" % "--cli=2.1")

let opam_container = Cmd.(v "opam-2.1" % "--cli=2.1")

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

module Mount = struct
  module StringMap = Map.Make (String)

  type t = {mount_type: string; options: string StringMap.t}

  let cache ?id ~user () =
    let options =
      if user then
        List.to_seq
          [
            ("uid", string_of_int Config.container_uid)
          ; ("gid", string_of_int Config.container_gid)
          ]
        |> StringMap.of_seq
      else
        StringMap.empty
    in
    let options =
      match id with None -> options | Some id -> StringMap.add "id" id options
    in
    {mount_type= "cache"; options}

  let bind_ro ?from source =
    let options = StringMap.singleton "source" Fpath.(to_string source) in
    let options =
      match from with
      | Some from ->
          StringMap.add "from" from options
      | None ->
          options
    in
    (* TODO: does podman need z or Z here? *)
    (* TODO: rw means discard writes, still RO from host's perspective,
     needed for config.mk *)
    {mount_type= "bind,rw"; options}

  let tmpfs = {mount_type= "tmpfs"; options= StringMap.empty}

  let to_string (target, t) =
    StringMap.add "target" Fpath.(to_string target) t.options
    |> StringMap.to_seq
    |> Seq.map (fun (k, v) -> Printf.sprintf "%s=%s" k v)
    |> List.of_seq
    |> String.concat ~sep:","
    |> Printf.sprintf "--mount=type=%s,%s" t.mount_type
end

module Layer = struct
  module PathMap = Map.Make (Fpath)

  type t = {
      mounts: Mount.t PathMap.t
    ; first: Cmd.t
    ; run_as_root: bool
    ; rest: Dockerfile.t list
  }

  let empty =
    {mounts= PathMap.empty; first= Cmd.empty; rest= []; run_as_root= false}

  let noop = Cmd.(v ":")

  let of_dockerfile t = {empty with rest= [t]}

  let to_dockerfile t =
    let mounts =
      t.mounts
      |> PathMap.to_seq
      |> Seq.map Mount.to_string
      |> List.of_seq
      |> String.concat ~sep:" "
    in
    let first_cmd =
      Cmd.to_string (if Cmd.is_empty t.first then noop else t.first)
    in
    let run_cmds =
      Dockerfile.(run "%s %s" mounts first_cmd @@@ t.rest |> crunch)
    in
    if t.run_as_root then
      Dockerfile.(user "root" @@@ [run_cmds; user "opam"])
    else if Cmd.is_empty t.first then (
      if not (PathMap.is_empty t.mounts) then
        invalid_arg "mounts cannot be combined with non RUN commands" ;
      Dockerfile.(empty @@@ t.rest |> crunch)
    ) else
      run_cmds

  let merge_mount path existing next =
    let existing_str = Mount.to_string (path, existing)
    and next_str = Mount.to_string (path, next) in
    if String.equal existing_str next_str then
      Some existing
    else
      Fmt.invalid_arg
        "Path %a mounted multiple times with different options:\n\
        \        %s <> %s" Fpath.pp path existing_str next_str

  let home = Fpath.(v "/home" / "opam")

  let with_mount ~target m t =
    let new_mount = PathMap.singleton Fpath.(home // target |> normalize) m in
    {t with mounts= PathMap.union merge_mount t.mounts new_mount}

  let v cmd = {empty with first= cmd}

  let arg ?default x = x |> Dockerfile.arg ?default |> of_dockerfile

  let env lst = lst |> Dockerfile.env |> of_dockerfile

  let dockerfile_of_cmd cmd = Dockerfile.run "%s" Cmd.(to_string cmd)

  let ( ++ ) a b =
    {
      mounts= PathMap.union merge_mount a.mounts b.mounts
    ; first= a.first
    ; rest=
        List.concat
          [
            a.rest
          ; ( if Cmd.is_empty b.first then
                []
            else
              [dockerfile_of_cmd b.first]
            )
          ; b.rest
          ]
    ; run_as_root= a.run_as_root || b.run_as_root (* TODO: reject incompat *)
    }

  let of_list = function [] -> empty | hd :: tl -> List.fold_left ( ++ ) hd tl

  let with_run_as_root t = {t with run_as_root= true}
end

module Stage = struct
  type t = {alias: string option; dockerfile: Dockerfile.t}

  let v ?alias ?tag ?platform from_image lst =
    {
      alias
    ; dockerfile=
        Dockerfile.(
          from ?alias ?tag ?platform from_image
          @@@ List.map Layer.to_dockerfile lst
        )
    }

  let to_dockerfile t = t.dockerfile

  (* TODO: doesn't quite work with podman, can't find dir on host from stage,
     apparently a buildah bug fixed very recently but not released yet
  *)
  let with_ro_mount ~from ~source ~target t =
    let open Layer in
    t
    |> with_mount ~target
       @@ Mount.(
            bind_ro ~from:(from.alias |> Option.get) Fpath.(home // source)
          )

  let copy_from ~from ~source ~target =
    let open Layer in
    Layer.of_dockerfile
    @@ Dockerfile.copy ~from:(Option.get from.alias)
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

let with_cache ~target ~user ?id t =
  t |> Layer.with_mount ~target @@ Mount.(cache ?id ~user ())

let with_root_cache =
  with_cache ~user:false ~target:Fpath.(v "/" / "var" / "cache")

let with_user_cache = with_cache ~user:true ~target:Fpath.(v ".cache")

let with_build_cache ~id = with_cache ~id ~user:true

let with_ro_mount ~uniqueid ~host_source ~target t =
  let open Layer in
  let mount =
    v Cmd.(noop % uniqueid) |> with_mount ~target @@ Mount.(bind_ro host_source)
  in
  mount ++ t

let yum_install packages =
  with_root_cache
  @@
  Layer.v Cmd.(v "sudo" % "yum" % "install" % "-y" %% of_list packages)

let installer_of_package_manager =
  let open Dockerfile_opam in
  let linux f packages =
    f (format_of_string "%s") Cmd.(of_list packages |> to_string)
    |> Layer.of_dockerfile
    |> with_root_cache
    |> Layer.with_run_as_root
  in
  let windows f packages = packages |> f |> Layer.of_dockerfile in
  (* TODO: our own so we can sudo, avoid yum clean, etc. *)
  function
  | `Apk ->
      linux Linux.Apk.install
  | `Apt ->
      linux Linux.Apt.install
  | `Yum ->
      yum_install
  | `Zypper ->
      linux Linux.Zypper.install
  | `Pacman ->
      linux Linux.Pacman.install
  | `Cygwin ->
      windows Windows.Cygwin.install
  | `Windows ->
      windows Windows.Winget.install

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
  Layer.v Cmd.(v "ln" % "-sf" % p link_source % p link_target)

let opam_install ?(repositories = []) packages =
  with_user_cache
  @@
  let link_source = Fpath.(v ".cache" / "opam" / "download-cache") in
  let link_target = Fpath.(v ".opam" / "download-cache") in
  let repo_cmds =
    match repositories with
    | [] ->
        Layer.empty
    | repos ->
        Cmd.(opam_container % "repo" % "remove" % "default")
        :: (repos
           |> List.map @@ fun (name, url) ->
              Cmd.(opam_container % "repo" % "add" % name % url)
           )
        |> List.map Layer.v
        |> Layer.of_list
  in
  Layer.(
    of_list
      [
        v Cmd.(v "rm" % p link_target % "-rf")
      ; v Cmd.(v "mkdir" % "-p" % p link_source)
      ; symlink ~link_source ~link_target
      ; repo_cmds
      ; v Cmd.(opam_container % "install" %% of_list packages)
      ]
  )

let dune_workspace = Fpath.v "workspace"

let monorepo_pull ~lockfile =
  let lockfile_dst = Fpath.(dune_workspace // base lockfile) in
  let digest = Digest.file Fpath.(to_string lockfile) |> Digest.to_hex in
  (* could use COPY, but that is based on timestamps,
     which won't work well with caching, a digest is more reliable *)
  with_ro_mount ~uniqueid:digest ~host_source:lockfile ~target:lockfile_dst
  @@ with_user_cache
  @@ Layer.(
       of_list
         [
           v
             Cmd.(
               opam_container
               % "monorepo"
               % "pull"
               % "-r"
               % p Fpath.(home // dune_workspace)
               % "-l"
               % p lockfile_dst
             )
         ]
     )

let dune_build ?(release = false) ~argname ~source_build_id targets =
  let uniqueid = "${" ^ argname ^ "}" in
  let target = Fpath.(dune_workspace / "source_ro") in
  let prefix = Fpath.(Layer.home / "prefix") in
  with_ro_mount ~uniqueid ~host_source:Fpath.(v ".") ~target
  @@ with_build_cache ~id:source_build_id
       ~target:Fpath.(dune_workspace / "_build")
  @@ with_user_cache
  @@ Layer.(
       of_list
         [
           v Cmd.(v "touch" % p Fpath.(dune_workspace / "dune-workspace"))
         ; v Cmd.(v "cd" % p target)
         (* FIXME: xapi specific *)
         ; v Cmd.(v "./configure" % "--prefix" % p prefix)
         ; v
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
         ; v
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
        ; v Cmd.(v "sudo" % "make" % "-C" % "scripts" % "install" % "DESTDIR=/")
         ; v Cmd.(v "cp" % p Fpath.(v "run.sh") % p Fpath.(home / "prefix" / "run.sh"))
         ]
     )

let duniverse_path = Fpath.(dune_workspace / "duniverse")

let dockerfile_duniverse_stage ~image ~lockfile =
  (* install monorepo into a container build stage of its own,
     so it doesn't interfere with installing deps of our repo.
     Run monorepo pull in that layer, and then copy it over.
  *)
  Stage.v ~alias:"duniverse" image
    Layer.
      [
        (* TODO: hardlink doesn't quite yet work yet, EXDEV *)
        env [("DUNE_CACHE", "enabled"); ("DUNE_CACHE_STORAGE_MODE", "copy")]
      ; opam_install ["opam-monorepo"]
      ; v Cmd.(v "mkdir" % "-p" % p dune_workspace)
      ; monorepo_pull ~lockfile
      ]


let main =
  if debug then Logs.set_level ~all:true (Some Logs.Debug) ;
  distro_result >>= fun distro ->
  lockfile_result >>= fun lockfile ->
  let installer =
    distro
    |> Dockerfile_opam.Distro.package_manager
    |> installer_of_package_manager
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
      Layer.
        [
          (* package that installs additional repos must be separate *)
          v
            Cmd.(
              v "sudo"
              % "yum"
              % "install"
              % "-y"
              % "epel-release"
              % "centos-release-xen"
            )
        ; installer t.opam_depexts
          (* minimal system deps just for opam packages *)
        ; env [("DUNE_CACHE", "enabled"); ("DUNE_CACHE_STORAGE_MODE", "copy")]
        ; v Cmd.(v "mkdir" % "-p" % p dune_workspace)
          (* TODO: might need storage mode copy if we get EXDEV from the cache *)
        ; opam_install ?repositories t.opam_packages
          (* install opam provided packages, these change rarely *)
        ; installer t.all_depexts
          (* install depexts for duniverse+local packages, avoids recompiling opam packages if this changes *)
        ; Layer.arg argname
        ; Stage.copy_from ~from:duniverse_stage ~source:duniverse_path
            ~target:duniverse_path
        ; dune_build ~argname ~release:true ~source_build_id build_targets
        ]

  in

  Containerfile.output [duniverse_stage; main_stage]

let () = R.failwith_error_msg main
