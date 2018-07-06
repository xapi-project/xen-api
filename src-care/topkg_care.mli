(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Topkg package care.

    Tools to help the package developer in the life cycle of the
    package. Most of these tools can be invoked directly from the
    command line via the [topkg] binary installed by the [topkg-care]
    package.

    {b WARNING.} Do not use this API in your package description file, use
    only {!Topkg}. This API was not thoroughly designed, is not stable and
    may change even between minor versions of [topkg]. Use at your own
    risk.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

open Bos_setup

(** {1 Helpers} *)

(** Text processing helpers. *)
module Text : sig

  (** {1 Marked-up text files}

      {b Warning.} Some of the following functions are not serious and can
      break on certain valid inputs in all sorts of fashion. To understand
      breakage bear in mind that they operate line-wise. *)

  type flavour = [ `Markdown | `Asciidoc ]
  (** The type for text document formats. *)

  val flavour_of_fpath : Fpath.t -> flavour option
  (** [flavour_of_fpath p] determines a flavour according to the
      extension of [p] as follows:
      {ul
      {- [Some `Markdown] for [.md]}
      {- [Some `Asciidoc] for [.asciidoc] or [.adoc]}
      {- [None] otherwise}} *)

  val head : ?flavour:flavour -> string -> (string * string) option
  (** [head ~flavour text] extracts the {e head} of the document [text] of
      flavour [flavour] (defaults to [`Markdown]).

      The head is defined as follows:
      {ul
      {- Anything before the first header is discarded.}
      {- The first header is kept in the first component}
      {- Everything that follows until the next header of the same or greater
         level is kept discarding trailing blank lines.}} *)

  val header_title : ?flavour:flavour -> string -> string
  (** [header_title ~flavour text] extract the title of a header [text]
      of flavour [flavour] (defaults to [`Markdown]). *)

  (** {1 Toy change log parsing} *)

  val change_log_last_entry :
    ?flavour:flavour -> string -> (string * (string * string)) option
  (** [change_log_last_version ~flavour text] tries to parse the last
      change log entry of [text] (i.e. the {!head} of [text]) into
      [Some (version, (header, text))], where [(header,text)] is the
      result of {!head} and [version] a version number extracted from
      [header] (see [topkg-log(2)] for details). *)

  val change_log_file_last_entry :
    Fpath.t -> ((string * (string * string)), R.msg) result
  (** [change_log_file_last_entry file] tries to parse the last
      change log entry of the file [file] using {!flavour_of_fpath} and
      and {!change_log_last_entry}. *)

  (** {1 Toy URI parsing} *)

  val split_uri : ?rel:bool -> string -> (string * string * string) option
  (** [split_uri uri] splits [uri] into a triple [(scheme, host, path)]. If
      [rel] is [true] (defaults to [false]), a leading ["/"] in [path] is
      removed. *)

  (** {1 Edit and page text} *)

  val edit_file : Fpath.t -> (int, R.msg) result
  (** [edit_file f] invokes the tool mentioned in the [EDITOR]
      environment variable with [f] and returns the exit code of
      the program. *)

  val find_pager : don't:bool -> (Cmd.t option, R.msg) result
  (** [find ~no_pager] is an optional pager command. If [don't] is
      [true] returns [None]. Otherwise first consults the [PAGER] environment
      variable, then tries [less] or [more] in that order. If the [TERM]
      environment variable is ["dumb"] or undefined unconditionaly returns
      [None]. *)
end

(** Pretty printers. *)
module Pp : sig

  (** {1 Pretty printers} *)

  val name : string Fmt.t
  (** [name] formats a package name. *)

  val version : string Fmt.t
  (** [version] formats a package version. *)

  val commit : string Fmt.t
  (** [commit] formats a commit-ish. *)

  val dirty : unit Fmt.t
  (** [dirty] formats a "dirty" string. *)

  val path : Fpath.t Fmt.t
  (** [path] formats a bold path *)

  val status : [`Ok | `Fail] Fmt.t
  (** [status] formats a result status. *)
end

(** [opam] helpers. *)
module Opam : sig

  (** {1:cmd Command} *)

  val cmd : Cmd.t
  (** [cmd] is a command for [opam] looked up using
      {!Topkg.Conf.tool}[ "opam" `Host_os]. *)

  (** {1:publish Publish} *)

  val ensure_publish : unit -> (unit, R.msg) result
  (** [ensure_publish ()] makes sure [opam-publish] is in the executable
      search PATH. *)

  val submit : ?msg:string -> pkg_dir:Fpath.t -> (unit, R.msg) result
  (** [submit ~pkg_dir] submits the package [pkg_dir] with [opam-publish]
      and submission message [msg] (if any) to the OCaml opam repository. *)

  (** {1:pkgs Packages} *)

  val ocaml_base_packages : String.set
  (** [ocaml_base_packages] are the base opam packages distributed
      with OCaml: ["base-bigarray"], ["base-bytes"], ["base-threads"],
      ["base-unix"]. *)

  (** {1:file Files} *)

  (** opam files *)
  module File : sig

    (** {1:file opam file} *)

    val field_names : String.set
    (** [field_names] is the maximal domain of the map returned by
        {!fields}, excluding extension fields (not yet supported by
        [opam-lib] 1.2.2). *)

    val fields : Fpath.t -> ((string list) String.map , R.msg) result
    (** [fields f] returns a simplified model of the fields of the opam
        file [f]. The domain of the result is included in
        {!field_names}. Note that the [depends:] and [depopts:] fields
        are returned without version constraints. *)

    (** {1:deps Dependencies} *)

    val deps : ?opts:bool -> (string list) String.map -> String.set
    (** [deps ~opts fields] returns the packages mentioned in the [depends:]
        fields, if [opts] is [true] (default) those from [depopts:] are added
        aswell. *)
  end

  (** [descr] files. *)
  module Descr : sig

    (** {1:descr Descr file} *)

    type t = string * string
    (** The type for opam [descr] files, the package synopsis and the
        description. *)

    val of_string : string -> (t, R.msg) result
    (** [of_string s] is a description from the string [s]. *)

    val to_string : t -> string
    (** [to_string d] is [d] as a string. *)

    val of_readme :
      ?flavour:Text.flavour -> string -> (t, R.msg) result
    (** [of_readme r] extracts an opam description file from a readme [r]
        with a certain structure. *)

    val of_readme_file : Fpath.t -> (t, R.msg) result
    (** [of_readme_file f] extracts an opam description file from
        a readme file [f] using {!Text.flavour_of_fpath} and
        {!of_readme}. *)
  end

  (** [url] files. *)
  module Url : sig

    (** {1:url Url file} *)

    val v : uri:string -> checksum:string -> string
    (** [v ~uri ~checksum] is an URL file for URI [uri] with
        checksum [checksum]. *)

    val with_distrib_file : uri:string -> Fpath.t -> (string, R.msg) result
    (** [with_distrib_file ~uri f] is an URL file for URI [uri] with
        the checksum of file [f]. *)
  end
end

(** [ocamlbuild] helpers. *)
module OCamlbuild : sig

  (** {1:cmd Command} *)

  val cmd : Cmd.t
  (** [cmd] is a command for [ocamlbuild] looked up using
      {!Topkg.Conf.tool}[ "ocamlbuild" `Host_os]. *)

  (** {1 Packages} *)

   val package_tags : ?roots:bool -> Fpath.t -> (String.set, R.msg) result
   (** [packages ~roots f] is the set of packages identifiers
       mentioned by the
       {{:https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc#17-findlib-based-packages}
       package tags} of the [_tags] file [f]. If [roots] is [true]
       (defaults to [false]) only root packages, i.e. the identifier
       before the first ['.'], are in the set.

       {b Warning.} This is a very dumb parsing that simply looks up
       for all ["package($ID)"] and ["package($ID0[, ]*$ID1...)"]
       patterns in the [_tags] file. *)
end

(** [ocamlfind] helpers. *)
module OCamlfind : sig

  (** {1:cmd Command} *)

  val cmd : Cmd.t
  (** [cmd] is a command for [ocamlfind] looked up using
      {!Topkg.Conf.tool}[ "ocamlfind" `Host_os]. *)

  (** {1 Packages} *)

  val base_packages : String.set
  (** [base_packages] are the OCamlfind packages that are
       distributed with OCaml. *)
end

(** Archive file creation. *)
module Archive : sig

  (** {1 Ustar archives} *)

  val tar :
    Fpath.t -> exclude_paths:Fpath.set -> root:Fpath.t -> mtime:int ->
    (string, R.msg) result
  (** [tar dir ~exclude_paths ~root ~mtime] is a (us)tar archive that
      contains the file hierarchy [dir] except the relative
      hierarchies present in [exclude_paths]. In the archive, members
      of [dir] are rerooted at [root] and sorted according to
      {!Fpath.compare}. They have their modification time set to
      [mtime] and their file permissions are [0o775] for directories
      and files executable by the user and [0o664] for other files. No other
      file metadata is preserved.

      {b Note.} This is a pure OCaml implementation, no [tar] tool is needed. *)

  (** {1 Bzip2 compression and unarchiving} *)

  val ensure_bzip2 : unit -> (unit, R.msg) result
  (** [ensure_bzip2 ()] makes sure the [bzip2] utility is available. *)

  val bzip2 : string -> dst:Fpath.t -> (unit, R.msg) result
  (** [bzip2 s dst] compresses [s] to [dst] using bzip2. *)

  val ensure_tar : unit -> (unit, R.msg) result
  (** [ensure_tar ()] makes sure the [tar] utility is available. *)

  val untbz : ?clean:bool -> Fpath.t -> (Fpath.t, R.msg) result
  (** [untbz ~clean ar] untars the tar bzip2 archive [ar] in the same
      directory as [ar] and returns a base directory for [ar]. If [clean] is
      [true] (defaults to [false]) first delete the base directory if it
      exists. *)
end

(** {1 Package care} *)

(** Package description. *)
module Pkg : sig

  (** {1 Packages} *)

  type t
  (** The type for package descriptions. *)

  val v :
    ?name:string ->
    ?version:string ->
    ?delegate:Cmd.t ->
    ?build_dir:Fpath.t ->
    ?opam:Fpath.t ->
    ?opam_descr:Fpath.t ->
    ?readme:Fpath.t ->
    ?change_log:Fpath.t ->
    ?license:Fpath.t ->
    ?distrib_uri:string ->
    ?distrib_file:Fpath.t ->
    ?publish_msg:string ->
    ?publish_artefacts:[ `Distrib | `Doc | `Alt of string] list ->
    Fpath.t -> t
  (** [v pkg_file] is a package from description file [pkg_file] which
      is loaded only if needed. The optional parameters allow to
      override [pkg_file]'s definition. *)

  val pkg_file : t -> Fpath.t
  (** [pkg_file p]  is [p]'s description file. *)

  val name : t -> (string, R.msg) result
  (** [name p] is [p]'s name. *)

  val version : t -> (string, R.msg) result
  (** [version p] is [p]'s version string.*)

  val delegate : t -> (Cmd.t, R.msg) result
  (** [delegate p] is [p]'s delegate. *)

  val build_dir : t -> (Fpath.t, R.msg) result
  (** [build_dir p] is [p]'s build directory. *)

  val opam : t -> (Fpath.t, R.msg) result
  (** [opam p] is [p]'s opam file. *)

  val opam_descr : t -> (Opam.Descr.t, R.msg) result
  (** [opam_descr p] is [p]'s opam description. *)

  val opam_field : t -> string -> (string list option, R.msg) result
  (** [opam_field p f] looks up field [f] of [p]'s opam file. *)

  val opam_fields : t -> (string list String.map, R.msg) result
  (** [opam_fields p] are [p]'s opam file fields. *)

  val readmes : t -> (Fpath.t list, R.msg) result
  (** [readmes p] are [p]'s readme files. *)

  val readme : t -> (Fpath.t, R.msg) result
  (** [readme p] is the first element of [readmes p]. *)

  val change_logs : t -> (Fpath.t list, R.msg) result
  (** [change_logs p] are [p]'s change logs. *)

  val change_log : t -> (Fpath.t, R.msg) result
  (** [change_log p] is the first element of [change_logs p]. *)

  val licenses : t -> (Fpath.t list, R.msg) result
  (** [licenses p] are [p]'s license files. *)

  val distrib_uri : ?raw:bool -> t -> (string, R.msg) result
  (** [distrib_uri p] is [p]'s distribution URI. If [raw] is [true]
      defaults to [false], [p]'s raw URI distribution pattern is returned. *)

  val distrib_file : t -> (Fpath.t, R.msg) result
  (** [distrib_file p] is [p]'s distribution archive. *)

  val publish_msg : t -> (string, R.msg) result
  (** [publish_msg p] is [p]'s distribution publication message. *)

  (** {1 Test} *)

  val test :
    t -> dir:Fpath.t -> args:Cmd.t ->
    out:(OS.Cmd.run_out -> ('a, R.msg) result) -> ('a, R.msg) result

  (** {1 Build} *)

  val build :
    t -> dir:Fpath.t -> args:Cmd.t ->
    out:(OS.Cmd.run_out -> ('a, R.msg) result) -> ('a, R.msg) result

  (** {1 Clean} *)

  val clean :
    t -> dir:Fpath.t -> args:Cmd.t ->
    out:(OS.Cmd.run_out -> ('a, R.msg) result) -> ('a, R.msg) result

  (** {1 Distribution} *)

  val distrib_archive : t -> keep_dir:bool -> (Fpath.t, R.msg) result
  (** [distrib_archive p ~keep_dir] creates a distribution archive
      for [p] and returns its path. If [keep_dir] is [true] the
      repository checkout used to create the distribution archive
      is kept in the build directory. *)

  val distrib_filename : ?opam:bool -> t -> (Fpath.t, R.msg) result
  (** [distrib_filename ~opam p] is a distribution filename for [p].
      If [opam] is [true] (defaults to [false]), the name follows
      opam's naming conventions. *)

  val publish_artefacts : t ->
    ([ `Distrib | `Doc | `Alt of string ] list, R.msg) result
  (** [publish_artefacts p] are [p]'s publication artefacts. *)

  (** {1 Lint} *)

  type lint = [ `Custom | `Std_files | `Meta | `Opam | `Deps ]
  (** The type for lints. *)

  val lint_all : lint list
  (** [lint_all] is a list with all lint values. *)

  val lint :
    ?ignore_pkg:bool -> t -> dir:Fpath.t -> lint list ->
    (int, R.msg) result
  (** [distrib ~ignore_pkg p ~dir lints] performs the lints
      mentioned in [lints] in a directory [dir] on the package [p].
      If [ignore_pkg] is [true] [p]'s definitions are ignored. *)
end

(** Package delegate. *)
module Delegate : sig

  (** {1 Publish} *)

  val publish_distrib :
    Pkg.t -> msg:string -> archive:Fpath.t -> (unit, R.msg) Result.result
  (** [publish_distrib p ~msg ~archive] publishes the distribution
      archive [archive] of package [p] with publication message [msg]. *)

  val publish_doc :
    Pkg.t -> msg:string -> docdir:Fpath.t -> (unit, R.msg) Result.result
  (** [publish_distrib p ~msg ~docdir] publishes the documentation
      directory [docdir] of package [p] with publication message [msg]. *)

  val publish_alt :
    Pkg.t -> kind:string -> msg:string -> archive:Fpath.t ->
    (unit, R.msg) Result.result
  (** [publish_alt p ~kind ~msg ~archive] publishes the
      artefact [kind] for distribution archive [archive] of package [p]
      with publication message [msg]. *)

  (** {2 Helpers} *)

  val publish_in_git_branch :
    remote:string -> branch:string ->
    name:string -> version:string -> docdir:Fpath.t ->
    dir:Fpath.t -> (unit, R.msg) result
  (** [publish_in_git_branch ~remote ~branch ~name ~version ~docdir ~dir]
      publishes the documentation directory [docdir] of a package
      named [name] at version [version] by replacing the [dir]
      sub-directory of the branch [branch] of the current working
      directory git repository (use ["."] to copy the docdir at the
      root directory of the branch) and pushes the branch to [remote].

      {b Note.} The publication procedure first checkouts the
      [gh-pages] in a temporary clone located in the {!Fpath.parent}
      directory of [docdir]. The [branch] branch of this clone is then
      pushed to the current working git repository, whose [branch]
      branch is then pushed to the [remote] repository. *)

  (** {1 Issues} *)

  val issue_list : Pkg.t -> (unit, R.msg) result
  (** [issue_list p] outputs the issue list on stdout. *)

  val issue_show : Pkg.t -> id:string -> (unit, R.msg) result
  (** [issue_show p ~id] outputs information about issue [id] on stdout. *)

  val issue_open : Pkg.t -> title:string -> body:string -> (unit, R.msg) result
  (** [issue_open p ~title ~body] create a new issue with title [title]
      and description body [body]. *)

  val issue_close : Pkg.t -> id:string -> msg:string -> (unit, R.msg) result
  (** [issue_close p ~id ~msg] closes issue [id] with message [msg]. *)
end


(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
