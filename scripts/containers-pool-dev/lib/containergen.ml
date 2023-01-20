open Bos

(** Source and container configuration *)
module Config = struct
  (** package to use for system dependency resolution *)
  let depext_package = "xs-toolstack"

  let deps_package = "xapi"

  let distro = "centos-7"

  let ocaml_major, ocaml_minor = (4, 13)

  (* depending how the container was built, not related to actual uid/gid from outside *)
  let container_uidgid = (1000, 1000)
end

let home = Fpath.v "/home/opam"

(** Cache and tmpfs mounts, see:
  https://docs.docker.com/engine/reference/builder/#run---mounttypecache
  https://www.mankier.com/5/Containerfile

  Supported by:
  * Docker BuildKit (included in newish versions of docker)
  * Podman 4.x
*)
module Command : sig
  (** whether multiple container builds can share this cache or not *)
  type sharing = Shared | Locked

  (** A Dockerfile RUN command with optional caching and redirections *)
  type t

  val with_cache :
       ?id:string
    -> ?sharing:sharing
    -> ?from:string
    -> ?source:Fpath.t
    -> ?uidgid:int * int
    -> ?mode:int
    -> target:Fpath.t
    -> t
    -> t
  (** [with_cache ?id ?sharing ?from ?source ?uidgid ?mode ~target cmd] foo

    @param id caches with the same id are shared across all (possibly
      unrelated) container builds. Defaults to [target] if not specified.
      The cache may have individual files GCed, don't store any state here.

    @param target the path inside the container where the cache is mounted.
      The container builder will typically use a directory on the host to store
      this, and start cleaning it up if it goes above a certain size.

    @from which build stage to initialize the cache with, default is empty directory
    @source path inside [from], defaults to its root

    @mode file mode for cache, default 0755

    @uidgid UID and GID for the cahe dir, defaults to 0,0

    @see <https://docs.docker.com/engine/reference/builder/#run---mounttypecache>
   *)

  val with_committed_cache :
       ?id:string
    -> ?sharing:sharing
    -> ?from:string
    -> ?source:Fpath.t
    -> ?uidgid:int * int
    -> ?mode:int
    -> target:Fpath.t
    -> Cmd.t
    -> t list
  (** [with_committed_cache] is like [with_cache], but also commits the final state of the target directory to the RUN layer.
      It uses [.cache/target] as the actual cache mount.
   *)

  val with_tmpfs : target:Fpath.t -> t -> t
  (** [with_tmpfs ~target cmd] mounts a temporary RAM filesystem inside the
      container.
      By default there is no upper limit imposed by the container builder,
      and the contents is lost after the RUN command.

  @param target path inside container

  @see <https://docs.docker.com/engine/reference/builder/#run---mounttypetmpfs>
  *)

  val with_bind : uniqueid:string -> source:Fpath.t -> target:Fpath.t -> t -> t
  (** [with_bind ~uniqueid ~source ~target cmd] mounts the [source] from the host inside the
      container. By default this is a read-only mount.

      (RW mounts are possible, but writes would be discarded, which would be
      confusing)
      The mount is only available for the duration of the [cmd], and not
      available when the container is running, i.e. its contents is not
      committed into the container's layers.
      This is useful if [source] is large or changes frequently.

      Because the contents of [source] is not copied into the container anymore
      it means that the container builder is no longer aware when it changes,
      and a [uniqueid] must be supplied, e.g. based on [git describe].

  @param uniqueid is embedded into the RUN command
  @param source path on the host that will be mounted
  @param target path inside container

  @see <https://docs.docker.com/engine/reference/builder/#run---mounttypebind>
  *)

  val v : ?stdin:Fpath.t -> ?stdout:Fpath.t -> Cmd.t -> t
  (** [v ?stdin ?stdout cmd] is [cmd] with standard input and output optionally
      redirected to [stdin] and [stdout] files inside the container.
   *)

  val run : t list -> Dockerfile.t
  (** [run lst] concatenates mounts and commands into a single RUN command
      to reduce number of layers.
   *)
end = struct
  type sharing = Shared | Locked

  module StringSet = Set.Make (String)

  type t = {
      mounts: StringSet.t
    ; cmd: Cmd.t
    ; uniqueid: StringSet.t
    ; stdin: Fpath.t option
    ; stdout: Fpath.t option
  }

  let v ?stdin ?stdout cmd =
    {mounts= StringSet.empty; cmd; stdin; stdout; uniqueid= StringSet.empty}

  let string_of_locking = function Shared -> "shared" | Locked -> "locked"

  let with_mount ?uniqueid typ ~target options cmd =
    let string_of_kv (k, v) =
      v |> Option.map @@ fun v -> Printf.sprintf "%s=%s" k v
    in
    let mount =
      ("target", Some Fpath.(to_string target)) :: options
      |> List.filter_map string_of_kv
      |> String.concat ","
      |> Printf.sprintf "--mount=type=%s,%s" typ
    in
    {
      cmd with
      mounts= StringSet.add mount cmd.mounts
    ; uniqueid=
        ( match uniqueid with
        | None ->
            cmd.uniqueid
        | Some u ->
            StringSet.add u cmd.uniqueid
        )
    }

  let with_cache ?id ?(sharing = Shared) ?from ?source ?uidgid ?mode =
    (* there is also 'ro' but not useful for us *)
    with_mount "cache"
      [
        ("id", id)
      ; ("sharing", Some (string_of_locking sharing))
      ; ("from", from)
      ; ("source", Option.map Fpath.to_string source)
      ; ("uid", Option.map fst uidgid |> Option.map string_of_int)
      ; ("gid", Option.map snd uidgid |> Option.map string_of_int)
      ; ("mode", Option.map (Printf.sprintf "%ou") mode)
      ]

  let with_tmpfs = with_mount "tmpfs" []

  let with_bind ~uniqueid ~source ~target cmd =
    with_mount "bind" ~uniqueid ~target
      [("source", Some Fpath.(to_string source))]
      cmd

  let redirect dir path =
    Option.map (fun p -> dir ^ Fpath.to_string p) path |> Option.to_list

  let to_run t =
    List.concat
      [
        t.mounts |> StringSet.elements
      ; [Cmd.to_string t.cmd]
      ; redirect "<" t.stdin
      ; redirect ">" t.stdout
      ]
    |> String.concat " "
    |> Dockerfile.run "%s"

  let run = function
    | [] ->
        Dockerfile.empty
    | first :: rest ->
        (* Dockerfile.crunch can't be used directly because it is not aware of mounts *)
        let mounts =
          List.fold_left StringSet.union first.mounts
          @@ List.map (fun t -> t.mounts) rest
        in
        let uniqueids =
          List.fold_left StringSet.union first.uniqueid
          @@ List.map (fun t -> t.uniqueid) rest
        in
        let cmd_unique =
          if StringSet.is_empty uniqueids then
            Cmd.empty
          else
            Cmd.(v "true" %% of_list (StringSet.elements uniqueids))
        in
        let cmds =
          {first with mounts}
          :: (v cmd_unique)
          :: List.map (fun t -> {t with mounts= StringSet.empty}) rest
        in
        cmds
        |> List.filter (fun x -> not @@ Cmd.is_empty x.cmd)
        |> List.map to_run
        |> List.fold_left Dockerfile.( @@ ) Dockerfile.empty
        |> Dockerfile.crunch

  let rsync ~src ~dst =
    Cmd.(
      v "rsync"
      % "-a"
      % "--whole-file"
      % "--no-compress"
      % p Fpath.(src / "")
      % p Fpath.(dst / "")
    )

  let with_committed_cache ?id ?sharing ?from ?source ?uidgid ?mode ~target cmd
      =
    let cache_target = Fpath.(home / ".cache" // base target) in
    let with_cache cmd =
      with_cache ?id ?sharing ?from ?source ?uidgid ?mode ~target:cache_target
      @@ v cmd
    in
    [
      with_cache (rsync ~src:cache_target ~dst:target)
    ; with_cache cmd
    ; with_cache (rsync ~src:target ~dst:cache_target)
    ]
end

module Dune = struct
  (** [with_dune cmd] mounts a dune cache during build.

    These are individual files that can be GCed at will.
   *)
  let with_dune cmd =
    (* only cache what we know to be individual files and sharable between all
       containers. *)
    Command.with_cache ~uidgid:Config.container_uidgid
      ~target:Fpath.(home / ".cache" / "dune")
    cmd

  let workspace = Fpath.(home / "workspace")

  (** [build ?watch ?release ~source build_targets]
    Performs a dune build containing source code in [source], mounted inside the container at [target].

    Creates a dune workspace, and mounts the sourcecode in a subdir,
    such that the [_build] directory will be a sibling, allowing [source] to be
    a read-only mount.

  *)
  let build ?(watch = false) ?(release = false) ~uniqueid ~source build_targets
      =
    let target = Fpath.(workspace / "source") in
    (* the _build is unique to the source we are building,
       shouldn't be shared with other containers, use the _build path that
       would've existed on the host as cache id.
    *)
    let id = Fpath.(source / "_build" |> to_string) in
    [ Command.v Cmd.(v "touch" % p Fpath.(workspace / "dune-workspace"))
    ; Command.v Cmd.(v "cd" % p target)
    ; with_dune
      @@ Command.with_bind ~uniqueid ~source ~target
      @@ Command.with_cache ~id ~sharing:Locked ~uidgid:Config.container_uidgid
           ~target:Fpath.(workspace / "_build")
      (*@@ Command.with_tmpfs ~target:Fpath.(v "/tmp") *)
      @@ Command.v
           Cmd.(
             v "opam"
             % "exec"
             % "--"
             % "dune"
             % "build"
             % "--cache"
             % "enabled"
             % "--cache-storage-mode"
             (* too many EXDEV failures for now with all the cache mounts *)
             % "copy"
             %% on watch (v "--watch")
             %% on release (v "--profile=release")
             %% of_list build_targets
           )
    ]
end

module Opam = struct
  let opam = Cmd.v "opam"

  let opam_from ?alias () =
    let open Config in
    Dockerfile.(
      parser_directive (`Syntax "docker/dockerfile:1")
      @@@ [
            Printf.sprintf "docker.io/ocaml/opam:%s-ocaml-%d.%d" distro
              ocaml_major ocaml_minor
            |> Dockerfile.from ?alias
            (* for [opam] builds, sometimes we need to build a dune package
               if it is in the middle of the dependency tree of a non-dune
               package
            *)
          ; env [("DUNE_CACHE", "enabled"); ("DUNE_CACHE_STORAGE_MODE", "copy")]
          (* needs to exist, otherwise when mounting the cache the parent dirs
           will be created with wrong permissions *)
          ; run "mkdir -p %s/.cache/dune/db" (Fpath.to_string home)
          ]
    )

  let with_opam_download_cache cmd =
    let sharing = Command.Locked in
    let target = Fpath.(home / ".opam" / "download-cache") in
    Dune.with_dune
    @@ Command.with_cache ~sharing ~target ~uidgid:Config.container_uidgid
    @@ cmd


  let switch_path = Fpath.(home / ".opam" / Printf.sprintf "%d.%d" Config.ocaml_major Config.ocaml_minor)

  let install ?(ignore_pin_depends = false) ?(deps_only = false)
      ?(locked = false) packages =
    (* Command.with_tmpfs ~target:Fpath.(v "/tmp") *)
    (* @@
    Command.with_tmpfs ~target:Fpath.(switch_path / ".opam-switch" / "build")
    *)
    with_opam_download_cache
    @@ Command.v
         Cmd.(
           opam
           % "install"
           %% on ignore_pin_depends (v "--ignore-pin-depends")
           %% on deps_only (v "--deps-only")
           %% of_list packages
           %% on locked (v "--locked")
         )

  let install_deps ~uniqueid ~lockfile =
    let target = Fpath.(Dune.workspace / basename lockfile) in
    Command.with_bind ~uniqueid ~source:lockfile ~target
    @@
    install ~ignore_pin_depends:true ~deps_only:true ~locked:true
      [Fpath.to_string target]

  let monorepo_pull ~uniqueid ~lockfile =
    let target = Fpath.(Dune.workspace / basename lockfile) in
    Command.with_bind ~uniqueid ~source:lockfile ~target
    @@
    with_opam_download_cache
    @@
    Command.v Cmd.(opam % "monorepo" % "pull" % "-r" % p Dune.workspace % "-l" % p target)
end

module Yum = struct
  let with_cache cmd =
    Command.with_cache ~sharing:Command.Locked
      ~target:Fpath.(v "/var/cache/yum")
      ~uidgid:(0, 0) cmd

  let install packages =
    with_cache
    @@ Command.v Cmd.(v "sudo" % "yum" % "install" % "-y" %% of_list packages)
end

module Generate = struct
  let stdout dockerfile = dockerfile |> Dockerfile.string_of_t |> print_endline
end
