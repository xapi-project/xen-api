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
  (** [with_cache ?id ?sharing ?from ?source ?uidgid ?mode ~target cmd]

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
  (** [with_tmpfs ~target cmd]

  @see <https://docs.docker.com/engine/reference/builder/#run---mounttypetmpfs>
  *)

  val with_bind : source:Fpath.t -> target:Fpath.t -> t -> t
  (** [with_bind ~source ~target]

  *)

  val v : ?stdin:Fpath.t -> ?stdout:Fpath.t -> Cmd.t -> t
  (** [v ?stdin ?stdout cmd] is [cmd] with standard input and output optionally
      redirected to [stdin] and [stdout] files.
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
    ; stdin: Fpath.t option
    ; stdout: Fpath.t option
  }

  let v ?stdin ?stdout cmd = {mounts= StringSet.empty; cmd; stdin; stdout}

  let string_of_locking = function Shared -> "shared" | Locked -> "locked"

  let with_mount typ ~target options cmd =
    let string_of_kv (k, v) =
      v |> Option.map @@ fun v -> Printf.sprintf "%s=%s" k v
    in
    let mount =
      ("target", Some Fpath.(to_string target)) :: options
      |> List.filter_map string_of_kv
      |> String.concat ","
      |> Printf.sprintf "--mount=type=%s,%s" typ
    in
    {cmd with mounts= StringSet.add mount cmd.mounts}

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

  let with_bind ~source ~target cmd =
    (* writes are discarded, but needed for _build *)
    with_mount "bind,rw" ~target [("source", Some Fpath.(to_string source))] cmd

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
        {first with mounts}
        :: List.map (fun t -> {t with mounts= StringSet.empty}) rest
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
  let with_dune cmd =
    Command.with_cache ~uidgid:Config.container_uidgid
      ~target:Fpath.(home / ".cache" / "dune")
      cmd

  let build ?(release = false) ~source ~target () =
    [
      Command.v Cmd.(v "cd" % p target)
    ; with_dune
      @@ Command.with_bind ~source ~target
      @@ Command.with_cache ~uidgid:Config.container_uidgid
           ~target:Fpath.(home // target / "_build")
      @@ Command.v
           Cmd.(
             v "opam"
             % "exec"
             % "--"
             % "dune"
             % "build"
             %% on release (v "--profile=release")
             % "xapi.install"
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
          ; env [("DUNE_CACHE", "enabled"); ("DUNE_CACHE_STORAGE_MODE", "copy")]
          ]
    )

  let with_opam_download_cache cmd =
    let sharing = Command.Locked in
    let target = Fpath.(home / ".opam" / "download-cache") in
    Dune.with_dune
    @@ Command.with_cache ~sharing ~target ~uidgid:Config.container_uidgid
    @@ cmd

  let install ?(ignore_pin_depends = false) ?(deps_only = false)
      ?(locked = false) packages =
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

  let monorepo_pull dir =
    if not (Fpath.is_abs dir) then invalid_arg (Fpath.to_string dir) ;
    Command.with_committed_cache
      ~target:Fpath.(dir / "duniverse")
      ~uidgid:Config.container_uidgid
    @@ Cmd.(opam % "monorepo" % "pull" % "-r" % p dir)
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
