(*---------------------------------------------------------------------------
   Copyright (c) 2016 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** The transitory OCaml package builder.

    See the {{!basics}basics} and the {{!menagerie}menagerie} of [pkg.ml]
    files.

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:prels Preliminaries}

    In the most simple cases you won't need this, jump directly to the
    {{!basics}basics} or {{!Pkg}package description API}. *)

val ( >>= ) :
  ('a, 'b) Result.result -> ('a -> ('c, 'b) Result.result) ->
  ('c, 'b) Result.result
(** [r >>= f] is [f v] if [r = Ok v] and [r] if [r = Error _]. *)

val ( >>| ) : ('a, 'b) Result.result -> ('a -> 'c) -> ('c, 'b) Result.result
(** [r >>| f] is [Ok (f v)] if [r = Ok v] and [r] if [r = Error _]. *)

(** This definition re-export [result]'s constructors so that an
    [open Topkg] gets them in scope. *)
type ('a, 'b) r = ('a, 'b) Result.result = Ok of 'a | Error of 'b

type 'a result = ('a, [ `Msg of string]) r
(** The type for topkg results. *)

(** Result value combinators. *)
module R : sig

  (** {1:err Errors} *)

  val reword_error : ('b -> 'c) -> ('a, 'b) Result.result ->
    ('a, 'c) Result.result
  (** [reword_error reword r] is:
      {ul
      {- [r] if [r = Ok v]}
      {- [Error (reword e)] if [r = Error e]}} *)

  (** {1:errmsg Error messages} *)

  type msg = [ `Msg of string ]
  (** The type for (error) messages. *)

  val error_msg : string -> ('b, [> msg]) Result.result
  (** [error_msg s] is [Error (`Msg s)]. *)

  val error_msgf :
    ('a, Format.formatter, unit, ('b, [> msg]) Result.result) format4 -> 'a
  (** [error_msgf fmt ...] is an error formatted according to [fmt]. *)

  val reword_error_msg :
    ?replace:bool -> (string -> msg) -> ('a, msg) Result.result ->
    ('a, [> msg]) Result.result
  (** [reword_error_msg ~replace reword r] is like {!reword_error} except
      if [replace] is [false] (default), the result of [reword old_msg] is
      concatened, on a new line to the old message. *)
end

val strf : ('a, Format.formatter, unit, string) format4 -> 'a
(** [strf] is [Printf.asprintf]. *)

(** Strings. *)
module String : sig

  (** {1:adds String additions} *)

  include module type of String

  val head : string -> char option
  (** [head s] if [Some s.[0]] if [s <> ""] and [None] otherwise. *)

  (** {1:preds Predicates} *)

  val is_prefix : affix:string -> string -> bool
  (** [is_prefix ~affix s] is [true] iff [affix.[i] = s.[i]] for
      all indices [i] of [affix]. *)

  val is_suffix : affix:string -> string -> bool
  (** [is_suffix ~affix s] is true iff [affix.[n - i] = s.[m - i]] for all
      indices [i] of [affix] with [n = String.length affix - 1] and [m =
      String.length s - 1]. *)

  val for_all : (char -> bool) -> string -> bool
  (** [for_all p s] is [true] iff for all indices [i] of [s], [p s.[i]
      = true]. *)

  val exists : (char -> bool) -> string -> bool
  (** [exists p s] is [true] iff there exists an index [i] of [s] with
      [p s.[i] = true]. *)

  (** {1:subs Extracting substrings} *)

  val with_index_range : ?first:int -> ?last:int -> string -> string
  (** [with_index_range ~first ~last s] are the consecutive bytes of [s]
      whose indices exist in the range \[[first];[last]\].

      [first] defaults to [0] and last to [String.length s - 1].

      Note that both [first] and [last] can be any integer. If
      [first > last] the interval is empty and the empty string is
      returned. *)

  val cut : ?rev:bool -> sep:char -> string -> (string * string) option
  (** [cut ~sep s] is either the pair [Some (l,r)] of the two
      (possibly empty) substrings of [s] that are delimited by the
      first match of the separator character [sep] or [None] if
      [sep] can't be matched in [s]. Matching starts from the
      beginning of [s] ([rev] is [false], default) or the end ([rev]
      is [true]).

      The invariant [l ^ (String.make 1 sep) ^ r = s] holds. *)

  val cuts : ?empty:bool ->  sep:char -> string -> string list
  (** [cuts ~sep s] is the list of all substring of [s] that are delimited by
      matches of [sep]. Empty substrings are ommited in the list if
      [empty] is [falsee] (defaults to [true]). The invariant
      [String.concat (String.make 1 sep) (split ~sep s) = s] holds. *)

  (** {1:vers Parsing version strings} *)

  val parse_version : string -> (int * int * int * string option) option
  (** [parse_version] parses version strings of the form:
{[
"[v]major.minor[.patchlevel][+additional-info]"
]}
      into [(major, minor, patch, additiona_info)] tuples. *)

  val drop_initial_v : string -> string
  (** [drop_initial_v s] drops a leading ['v'] or ['V'] from [s]. *)
end

type fpath = string
(** The type for file system paths. *)

(** File system paths.

    {b Note.} Only use ["/"] as a directory separator, even on
    Windows platforms. *)
module Fpath : sig

  (** {1:fpath File system paths} *)

  type t = fpath
  (** The type for file system paths. *)

  val append : t -> t -> t
  (** [append p q] appends [q] to [p] as follows:
      {ul
      {- If [q] is absolute then [q] is returned}
      {- Otherwise appends [q]'s segments to [p] using a ["/"] if needed.}} *)

  val ( // ) : t -> t -> t
  (** [p // q] is [append p q]. *)

  val is_dir_path : t -> bool
  (** [is_dir_path p] is [true] iff [p] represents a directory. This means
      that [p] is [.], [..] or ends with [/], [/..] or [/.]. *)

  val is_file_path : t -> bool
  (** [is_file_path p] is [not (is_dir_path true)]. *)

  val basename : t -> string
  (** [basename p] is [p]'s basename, the last non empty segment of [p]. *)

  val dirname : t -> string
  (** [dirname p] is [p]'s dirname, [p] without its  last non empty segment. *)

  (** {1:exts File extensions} *)

  val get_ext : t -> string
  (** [get_ext p] is [p]'s filename extension (including the ['.']) or
      the empty string if there is no extension *)

  val has_ext : string -> t -> bool
  (** [has_ext e p] is [true] iff [e] is a suffix of [p]. *)

  val rem_ext : t -> t
  (** [rem_ext p] is [p] without its filename extension. *)

end

(** Command lines.

    Both command lines and command line fragments using the same are
    represented with the same {{!t}type}.

    When a command line is {{!section:OS.Cmd.run}run}, the first
    element of the line defines the program name and each other
    element is an argument that is passed {e as is} in the
    program's [argv] array: no shell interpretation or any form of
    argument quoting and/or concatenation occurs. *)
module Cmd : sig

  (** {1:lines Command line fragments} *)

  type t
  (** The type for command line fragments. *)

  val v : string -> t
  (** [v cmd] is a new command line (or command line fragment)
      whose first argument is [cmd]. *)

  val empty : t
  (** [empty] is an empty command line. *)

  val is_empty : t -> bool
  (** [is_empty l] is [true] iff [l] is empty. *)

  val ( % ) : t -> string -> t
    (** [l % arg] adds [arg] to the command line [l]. *)

  val ( %% ) : t -> t -> t
  (** [l %% frag] appends the line fragment [frag] to [l]. *)

  val add_arg : t -> string -> t
  (** [add_arg l arg] is [l % arg]. *)

  val add_args : t -> t -> t
  (** [add_args l frag] is [l %% frag]. *)

  val on : bool -> t -> t
  (** [on bool line] is [line] if [bool] is [true] and {!empty}
      otherwise. *)

  val p : fpath -> string
  (** [p] is [(fun f -> f)]. *)

  (** {1:predicates Predicates and comparison} *)

  val equal : t -> t -> bool
  (** [equal l l'] is [true] iff [l] and [l'] are litterally equal. *)

  val compare : t -> t -> int
  (** [compare l l'] is a total order on command lines. *)

  (** {1:convert Conversions and pretty printing} *)

  val to_list : t -> string list
  (** [to_list l] is [l] as a list of strings. *)

  val of_list : ?slip:string -> string list -> t
  (** [of_list ?slip l] is a command line from the list of arguments
      [l].  If [slip] is specified it is added on the command line
      before each element of [l]. *)

  val dump : Format.formatter -> t -> unit
  (** [dump ppf cmd] formats an unspecified representation of [cmd] on
      [ppf]. *)
end

(** Topkg log. *)
module Log : sig

  (** {1:level Reporting levels} *)

  (** The type for reporting levels. *)
  type level = App | Error | Warning | Info | Debug

  val level : unit -> level option
  (** [level ()] is the current reporting level. *)

  val set_level : level option -> unit
  (** [set_level l] sets the current reporting level to [l]. *)

  val level_to_string : level option -> string
  (** [level_to_string l] converts [l] to an unspecified human-readable
      US-ASCII string that can be parsed back by {!level_of_string}. *)

  val level_of_string : string -> (level option, [`Msg of string]) Result.result
  (** [level_of_string s] parses the representation of {!level_to_string}
      from [s]. *)

  (** {1:logf Log functions} *)

  type 'a msgf =
    (?header:string ->
     ('a, Format.formatter, unit) Pervasives.format -> 'a) -> unit

  val msg : level -> 'a msgf -> unit
  (** [msg l (fun m -> m fmt ...)] logs with level [l] a message formatted
      with [fmt]. *)

  val app : 'a msgf -> unit
  (** [app] is msg [App]. *)

  val err : 'a msgf -> unit
  (** [err] is [msg Error]. *)

  val warn : 'a msgf -> unit
  (** [err] is [msg Warning]. *)

  val info : 'a msgf -> unit
  (** [err] is [msg Info]. *)

  val debug : 'a msgf -> unit
  (** [err] is [msg Debug]. *)

  val on_error_msg : ?level:level -> use:(unit -> 'a) -> 'a result -> 'a
  (** [on_error_msg ~level r] is:
      {ul
      {- [v] if [r = Ok v]}
      {- [use e] if [r = Error (`Msg e)]. As a side effect [e] is logged
         with level [level] (defaults to [Error]).}} *)

  (** {1:monitoring Monitoring} *)

  val err_count : unit -> int
  (** [err_count ()] is the number of messages logged with level [Error]. *)

  val warn_count : unit -> int
  (** [warn_count ()] is the number of messages logged with level [Warning]. *)
end

(** OS interaction. *)
module OS : sig

  (** {1:os OS} *)

  (** Environment variables *)
  module Env : sig

    (** {1:vars Variables} *)

    val var : string -> string option
    (** [var name] is the value of the environment variable [name], if
        defined. *)

    val opt_var : string -> absent:string -> string
    (** [opt_var name ~absent] is the value of the optionally defined
        environment variable [name], if defined, and [absent] if undefined. *)
  end

  (** File operations. *)
  module File : sig

    (** {1:paths Famous file paths} *)

    val null : fpath
    (** [null] represents a file on the OS that discards all writes
        and returns end of file on reads. *)

    val dash : fpath
    (** [dash] is ["-"]. This value is is used by {!read} and {!write}
        to respectively denote [stdin] and [stdout]. *)

    (** {1:exdel Existence and deletion} *)

    val exists : fpath -> bool result
    (** [exists file] is [true] if [file] is a regular in the file
        system and false otherwise. Symbolic links are followed. *)

    val must_exist : fpath -> fpath result
    (** [must_exist file] is [file] if [file] is a regular file in the
        file system and an error otherwise. Symbolic links are followed. *)

    val delete : ?must_exist:bool -> fpath -> unit result
    (** [delete ~must_exist file] deletes file [file]. If [must_exist]
        is [true] (defaults to [false]) an error is returned if [file]
        doesn't exist. *)

    (** {1:fold Folding over file hierarchies} *)

    val fold :
      ?skip:(fpath -> bool) -> (fpath -> 'a -> 'a) -> 'a ->
      fpath list -> 'a result
    (** [fold_files skip f acc paths] folds [f] over the {e files}
        found in the file hierarchies starting at [paths].  Files
        and directories [p] for which [skip p] is [true] are skipped.
        [skip] defaults to [(fun _ -> false)]. *)

    (** {1:rw Reading and writing} *)

    val read : fpath -> string result
    (** [read file] is [file]'s contents. If [file] is {!dash} reads
        from {!Pervasives.stdin} and the channel is not closed when
        the function returns. *)

    val write : fpath -> string -> unit result
    (** [write file content] writes [content] to [file]. If [file] is {!dash}
        writes to {!Pervasives.stdout} and flushes but doesn't close the channel
        when the function returns. *)

    val write_subst : fpath -> (string * string) list -> string -> unit result
    (** [write_subst file vars content] is like {!write} except any occurence
        of a string of the form ["%%ID%%"] in [content] is replaced by the
        value of [List.assoc "ID" vars], if any. *)

    (** {1:tmpfiles Temporary files} *)

    val tmp : unit -> fpath result
    (** [tmp ()] creates a temporary file and returns its name. The file
        is destroyed at the end of program execution. *)
  end

  (** Directory operations. *)
  module Dir : sig

    (** {1:exists Existence and contents} *)

    val exists : fpath -> bool result
    (** [exists dir] is [true] if directory [dir] exists in the file
        system. Symbolic links are followed. *)

    val must_exist : fpath -> fpath result
    (** [must_exist dir] is [dir] if [file] is a regular file in the
        file system and an error otherwise. Symbolic links are followed. *)

    val contents : ?dotfiles:bool -> ?rel:bool -> fpath -> fpath list result
    (** [contents ~dotfiles ~rel dir] is the list of directories and filse
        in [dir]. If [rel] is [true] (defaults to [false]) the resulting
        paths are relative to [dir], otherwise they have [dir] prepended.
        If [dotfiles] is [false] (default) elements that start with a [.]
        are omitted. *)

    (** {1:current Current working directory} *)

    val current : unit -> fpath result
    (** [current ()] is the current working directory. *)

    val set_current : fpath -> unit result
    (** [set_current dir] sets the current working directory to [dir]. *)

    val with_current : fpath -> ('a -> 'b) -> 'a -> 'b result
    (** [with_current dir f v] is [f v] with the current working directory
        bound to [dir]. After the function returns the current working
        directory is back to its initial value. *)
  end

  (** Running commands. *)
  module Cmd : sig

    (** {1:exists Command existence} *)

    val exists : Cmd.t -> bool result
    (** [exists cmd] is [true] if the executable of [cmd] can be found
        in the path and [false] otherwise. *)

    val must_exist : Cmd.t -> Cmd.t result
    (** [must_exist cmd] is [cmd] if the executable of [cmd] can be found
        in the path and an error otherwise. *)

    (** {1:run Running commands} *)

    val run : ?err:fpath -> Cmd.t -> unit result
    (** [run cmd] runs the command [cmd]. [std{i,o,err}] are connected
        to the invoking process' standard channels. If [err] is specified
        [stderr] is redirected to the given file (e.g. {!File.null}). *)

    val run_status : ?err:fpath -> Cmd.t -> [`Exited of int] result
    (** [run_status cmd] is like {!run}, but doesn't error on non-zero
        exit status. *)

    (** {1:stdout Capturing standard output} *)

    type run_status = Cmd.t * [`Exited of int ]
    (** The type for run statuses, the command that was run and the run
        status. *)

    val success : ('a * run_status) result -> 'a result
    (** [success r] is:
        {ul
        {- [Ok v] if [r = Ok (v, (_, `Exited 0))]}
        {- [Error _] otherwise. Non [`Exited 0] statuses are turned into
           an error message.}} *)

    type run_out
    (** The type for representing the standard output of a command run. *)

    val out_string : ?trim:bool -> run_out -> (string * run_status) result
    (** [out_string ~trim o] captures the standard output [o] as a [string].
        If [trim] is [true] (default) the result is passed through
        {!String.trim}. *)

    val out_lines : ?trim:bool -> run_out -> (string list * run_status) result
    (** [out_lines ~trim] is like {!out_string} but the result is
        {{!String.cuts}cut} on newlines (['\n']). *)

    val out_file : fpath -> run_out -> (unit * run_status) result
    (** [out_file f o] writes the standard output [o] to file [f]. *)

    val out_stdout : run_out -> (unit * run_status) result
    (** [out_stdout o] redirects the standard output [o] to the current
        process standard output. *)

    val to_string : ?trim:bool -> run_out -> string result
    (** [to_string] is [(out_string ?trim o |> success)]. *)

    val to_lines : ?trim:bool -> run_out -> string list result
    (** [to_lines ?trim o] is [(out_string ?trim o |> success)]. *)

    val to_file : fpath -> run_out -> unit result
    (** [to_file f o] is [(out_file f o |> success)] *)

    val run_out : ?err:fpath -> Cmd.t -> run_out
    (** [run_out cmd] represents the standard output of the command run [cmd].
        [std{i,err}] are connected to the invoking prcoess stream and standard
        output can be consumed with {!to_string}, {!to_lines} or {!to_file}.
        If [err] is specified [stderr] is redirected to the given file. *)
  end
end

(** Version control system repositories. *)
module Vcs : sig

  (** {1:vcsops Version control system repositories} *)

  type kind = [ `Git | `Hg ]
  (** The type for version control systems (VCS). *)

  val pp_kind : Format.formatter -> kind -> unit
  (** [pp_kind ppf k] prints an unspecified representation of [k] on [ppf]. *)

  type commit_ish = string
  (** The type for symbols resolving to a commit. The module uses
      ["HEAD"] for specifying the current checkout; use
      this symbol even if the underlying VCS is [`Hg]. *)

  type t
  (** The type for version control systems repositories. *)

  val kind : t -> kind
  (** [kind r] is [r]'s VCS kind. *)

  val dir : t -> fpath
  (** [dir r] is [r]'s repository directory. *)

  val cmd : t -> Cmd.t
  (** [cmd r] is the base VCS command to use to act on [r].

      {b Warning} Prefer the functions below to remain VCS independent. *)

  val find : ?dir:fpath -> unit -> t option result
  (** [find ~dir ()] looks for a VCS repository in working directory [dir]
      (not the repository directory like [.git], default is guessed
       automatically). *)

  val get : ?dir:fpath -> unit -> t result
  (** [get] is like {!find} but returns an error if no VCS was found. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf r] prints an unspecified representation of [r] on [ppf]. *)

  (** {1:state Repository state} *)

  val is_dirty : t -> bool result
  (** [is_dirty r] is [Ok true] iff the working tree of [r] has uncommited
      changes. *)

  val not_dirty : t -> unit result
  (** [not_dirty] is [Ok ()] iff the working directory of [r] is not dirty and
      an error that enjoins to stash or commit otherwise. *)

  val file_is_dirty : t -> fpath -> bool result
  (** [file_id_dirty r f] is [Ok true] iff [f] has uncommited changes. *)

  val head : ?dirty:bool -> t -> string result
  (** [head ~dirty r] is the HEAD commit identifier of the repository
      [r]. If [dirty] is [true] (default), and indicator is appended to
      the commit identifier if the working tree of [r] {!is_dirty}. *)

  val commit_id : ?dirty:bool -> ?commit_ish:commit_ish -> t -> string result
  (** [commit_id ~dirty ~commit_ish r] is the object name (identifier)
      of [commit_ish] (defaults to ["HEAD"]). If [commit_ish] is
      ["HEAD"] and [dirty] is [true] (default) and indicator is
      appended to the identifier if the working tree is dirty. *)

  val commit_ptime_s : ?commit_ish:commit_ish -> t -> int result
  (** [commit_ptime_s t ~commit_ish] is the POSIX time in seconds
      of commit [commit_ish] (defaults to ["HEAD"]) of repository [r]. *)

  val describe : ?dirty:bool -> ?commit_ish:commit_ish -> t -> string result
  (** [describe ~dirty ~commit_ish r] identifies [commit_ish] (defaults to
      ["HEAD"]) using tags from the repository [r]. If [commit_ish] is
      ["HEAD"] and [dirty] is [true] (default) an indicator is appended to
      the identifier if the working tree is dirty. *)

  val tags : t -> string list result
  (** [tags r] is the list of tags in the repo [r]. *)

  val changes :
    ?until:commit_ish -> t -> after:commit_ish -> (string * string) list result
  (** [changes r ~after ~until] is the list of commits with their
      one-line message from commit-ish [after] to commit-ish [until]
      (defaults to ["HEAD"]). *)

  val tracked_files : ?tree_ish:string -> t -> fpath list result
  (** [tracked_files ~tree_ish r] are the files tracked by the tree object
      [tree_ish] (defaults to ["HEAD"]). *)

  (** {1:ops Repository operations} *)

  val clone : t -> dir:fpath -> unit result
  (** [clone r ~dir] clones [r] in directory [dir]. *)

  val checkout : ?branch:string -> t -> commit_ish:commit_ish -> unit result
  (** [checkout r ~branch commit_ish] checks out [commit_ish]. Checks out
      in a new branch [branch] if provided. *)

  val commit_files : ?msg:string -> t -> fpath list -> unit result
  (** [commit_files r ~msg files] commits the file [files] with message
      [msg] (if unspecified the VCS should prompt). *)

  val tag :
    ?force:bool -> ?sign:bool -> ?msg:string -> ?commit_ish:string -> t ->
    string -> unit result
  (** [tag r ~force ~sign ~msg ~commit_ish t] tags [commit_ish] with [t]
      and message [msg] (if unspecified the VCS should prompt).  if
      [sign] is [true] (defaults to [false]) signs the tag ([`Git] repos only).
      If [force] is [true] (default to [false]) doesn't fail if the tag
      already exists. *)

  val delete_tag : t -> string -> unit result
  (** [delete_tag r t] deletes tag [t] in repo [r]. *)
end

(** {1:pkgdescr Package description} *)

(** Build configuration. *)
module Conf : sig

  (** {1:kconv Key value converters} *)

  type 'a conv
  (** The type for key value converters. *)

  val conv :
    ?docv:string -> (string -> 'a result) -> (Format.formatter -> 'a -> unit) ->
    'a conv
  (** [conv ~docv parse print] is a configuration value converter
      parsing values with [parse] and printing them with
      [print]. [docv] is a documentation meta-variable used in the
      documentation to stand for the configuration value, defaults to
      ["VALUE"]. *)

  val bool : bool conv
  (** [bool] is a converter for booleans. *)

  val int : int conv
  (** [int] is a converter for integers. *)

  val string : string conv
  (** [string] is a converter for strings. *)

  val fpath : fpath conv
  (** [fpath] is a converter for file paths. *)

  val some : ?none:string -> 'a conv -> 'a option conv
  (** [some conv] is like [conv] but wraps the parsed value in [Some].
      [none] is the string printed for [None] by the converter printer,
      defaults to [""]. *)

  (** {1:key Keys}

      {b Warning.} Configuration keys must be created before the call
      to {!Pkg.describe}. Yes you are right, that's a little bit dirty. *)

  type 'a key
  (** The type for configuration keys whose lookup value is of type ['a].

      A configuration key has a name and represents a value of type
      ['a] in a build configuration. If ["name"] is the name of the key
      then its value can be specified on the command line using
      [--name v] where [v] is the value of the key and is parsed
      according to the key's {{!conv}value converter}.

      There are a few predefined key, see the {{!conf}configuration section}. *)

  val key :
    ?docv:string -> ?doc:string -> ?env:string -> string -> 'a conv ->
    absent:'a -> 'a key
  (** [key name conv ~absent ~env ~doc ~docv] is a configuration key
      with name [name] parsed from the command line with [conv].
      [absent] is used if the value is not specified on the command
      line. If [env] is specified and exists in the process environment,
      the value of the variable is parsed with [conv] and used instead
      of [absent] when needed.

      [doc] is a documentation string for the key. [docv] is a documentation
      meta-variable to stand for the key value, defaulting to the
      [docv] of [conv]. In [doc], occurences of the substring ["$(docv)"]
      are replaced by the value of [docv]. *)

  val discovered_key :
    ?docv:string -> ?doc:string -> ?env:string -> string -> 'a conv ->
    absent:(unit -> 'a result) -> 'a key
  (** [discovered_key] is like {!key} but the absent value is discovered,
      {e if needed}, with [absent]. *)

  val with_pkg : ?default:bool -> string -> bool key
  (** [with_pkg ~default pkg] is a boolean configuration key named
      [(strf "with-%s" pkg)] to assert existence of opam packages.
      If absent defaults to [default].

      Usually specified in opam build instructions with:
      {["--with-thatpkg" thatpkg:installed]} along with an entry in the
      depopt field of the opam file.

      {b Warning.} Only use this combinator for denoting opam
      package existence, the resulting key may switch to a discovery
      process in the future. *)

  (** {1:conf Configurations} *)

  type t
  (** The type for configurations. *)

  val value : t -> 'a key -> 'a
  (** [value c k] is the value of configuration key [k] in [c].

      @raise Invalid_argument  If [k] was (illegaly) created after the call
      to {!Pkg.describe} or if dirty tricks are being played. *)

  val pkg_name : t -> string
  (** [pkg_name c] is either the value of the package name as given to
      {!Pkg.describe} or the value of a predefined key [--pkg-name] which
      overrides the package name. This defines the name of the generated
      opam install file. Used to handle {{!multiopam}multiple
      opam packages}. *)

  val build_dir : t -> fpath
  (** [build_dir c] is either the value of build directory as given
      to {!Pkg.describe} via {!Pkg.build} or the value of a predefined
      key [--build-dir] which overrides the package definition. *)

  val vcs : t -> bool
  (** [vcs c] is the value of a predefined key [--vcs].
      It is [true] if the package directory is VCS managed. Usually
      should not be specified manually: if absent it is determined
      automatically by using {!Topkg.Vcs.find} and used to determine
      the {!build_context}. *)

  val dev_pkg : t -> bool
  (** [dev_pkg c] is the value of a predefined key [--dev-pkg].
      It is [true] if the build is initiated by an installer like opam
      and the package sources are a checkout from a VCS rather
      than a distribution archive. Usually specified in opam build instruction
      with either:
{[
"--dev-pkg" "%{dev}%"    # for opam >= 2.0
"--dev-pkg" "%{pinned}%" #           < 2.0
]}
*)

  val pinned : t -> bool
  (** @deprecated use {!dev_pkg}

      [pinned c] is the value of a predefined key [--pinned]. It has
      exactly the same semantics as {!dev_pkg} but is misnamed. *)

  val jobs : t -> int
  (** [jobs c] is the value of a predefined key [--jobs].
      If absent it is determined from the build context as follows.
      {ul
      {- [`Dev] builds default to number of CPU cores, or 4 if it cannot be determined.}
      {- all other contexts default to 4}} *)

  type build_context = [`Dev | `Distrib | `Pin ]
  (** The type for build contexts. See {!val:build_context} for semantics. *)

  val build_context : t -> [`Dev | `Distrib | `Pin ]
  (** [build_context c] is the build context of [c]. This is derived from
      {!vcs} and {!dev_pkg} (or the deprecated {!pinned}) as follows.
      {ul
      {- [`Distrib] iff [not (vcs c)]. No VCS is present, this is a build from
         a distribution. If there are configuration bits they should
         be setup according to the build configuration.}
      {- [`Dev] iff [vcs c && not (dev_pkg c || pinned c)]. This is a
         development build invoked manually in a source repository. The
         repository checkout should likely not be touched and configuration
         bits not be setup. This is happening for example if the developer
         is testing the package description in her working source repository
         by invoking [pkg/pkg.ml] or [topkg build].}
      {- [`Pin] iff [vcs c && (dev_pkg c || pinned c)]. This is a package
         manager development build. In this case the repository checkout may
         need to be massaged into a pseudo-distribution for the package to be
         installed. This means that distribution watermarking and massaging
         should be performed, see {!Pkg.distrib} and the [prepare_on_pin]
         argument of {!Pkg.build}. Besides exisiting configuration bits
         should be setup according to the
         build environment. {b Note.} This is called [`Pin] due to a blind
         spot, a more approriate name would be something like [`Dev_pkg]
         build.}} *)

  val build_tests : t -> bool
  (** [build_tests c] is the value of a predefined key [--tests] that
      indicates if the tests should be built. If absent the value
      depends on the {!build_context}, it is [true] in the [`Dev]
      context and [false] in the other contexts. *)

  val debug : t -> bool
  (** [debug c] is the value of a predefined key [--debug] that
      indicates if the build should include debugging information
      in the build artefacts. If absent the value is [true] or the
      value of the environment variable TOPKG_CONF_DEBUG if
      specified. *)

  val debugger_support : t -> bool
  (** [debugger_support c] is the value of a predefined key
      [--debugger-support] that indicates if build artefacts needed
      for source level debuggers should be built and installed. If
      absent the value is [false] or the value of the environment
      variable TOPKG_CONF_DEBUGGER_SUPPORT if specified. *)

  val profile : t -> bool
  (** [profile c] is the value of a predefined key [--profile] that
      indicates if the build artefacts include run-time profiling support.  If
      absent the value is [false] or the value of the environment variable
      TOPKG_CONF_PROFILE if specified. *)

  val toolchain : t -> string option
  (** [toolchain c] is the value of a predefined key [--toolchain] that
      specifies the ocamlbuild toolchain. If absent the value is [None] or
      the value of the environment variable TOPKG_CONF_TOOLCHAIN if
      specified. *)

  val dump : Format.formatter -> t -> unit
  (** [dump ppf c] formats an unspecified representation of [c] on [ppf]. *)

  (** {1:tool Tool lookup}

      If your package description needs to run tools (e.g. in the
      pre and post build hooks, see {!Pkg.build}) it should get the
      tool to invoke with the following function. This allows to
      control the executable being run which is useful for
      cross-compilation scenarios. *)

  type os = [ `Build_os | `Host_os ]
  (** The type for operating systems.
      {ul
      {- [`Build_os] is the build OS, the OS on which the package is built.}
      {- [`Host_os] is the host OS, the OS on which the package is hosted
         and runs.}} *)

  val tool : ?conf:t -> string -> os -> Cmd.t
  (** [tool ~conf cmd os] is a command [cmd] which can be run on the build OS
      which produces output suitable for the OS [os], taking into account
      configuration [conf].

      The actual command returned depends on the following lookup procedure,
      here exemplified on the invocation [tool "mytool" `Host_os] (resp.
      [`Build_os]).
      {ol
      {- [Cmd.v "cmd"], if the environment variable HOST_OS_MYTOOL (resp.
         BUILD_OS_MYTOOL) is set to ["cmd"]}
      {- [Cmd.v (Fpath.append path "mytool")], if the environment variable
         HOST_OS_XBIN  (resp. BUILD_OS_BIN) is set to [path].}
      {- [Cmd.v ("mytool" ^ "suff")], if the environment variable
         HOST_OS_SUFF (resp. BUILD_OS_SUFF).}
      {- [Cmd.(v "ocamlfind" % "-toolchain" % "toolchain" % "mytool")] if
         ["mytool"] is part of the OCaml tools that can be invoked through
         ocamlfind, [toolchain conf] is not [None], and [os] is [`Host_os].}
      {- [Cmd.(v "ocamlfind" % "mytool")] if ["mytool"] is part of
         the OCaml tools that can be invoked through ocamlfind.}
      {- [Cmd.v "mytool"] otherwise.}} *)

  (** {1:ocaml OCaml configuration} *)

  (** OCaml configuration. *)
  module OCaml : sig

    (** {1:conf OCaml system configuration} *)

    type conf = t

    type t
    (** The type for OCaml configurations. *)

    val v : conf -> os -> t
    (** [v c os] is the configuration of the OCaml system for the OS
        [os] obtained by reading the output of [tool "ocamlc" os] with
        the [-config] option. *)

    val find : string -> t -> string option
    (** [find k c] looks up key [k] in configuration [c]. *)

    val version : t -> int * int * int * string option
    (** [version] is the compiler version string
        ["major.minor[.patchlevel][+additional-info]"] parsed into
        [(major, minor, patch, additional-info)]. *)

    val ext_asm : t -> string
    (** [ext_asm] is the file extension for assembly files, e.g. [".s"]. *)

    val ext_obj : t -> string
    (** [ext_asm] is the file extension for C object files, e.g. [".o"]. *)

    val ext_lib : t -> string
    (** [ext_lib] is the file extension for C static libraries, e.g. [".a"]. *)

    val ext_dll : t -> string
    (** [ext_dll] is the file extension for C dynamically linked libraries,
        e.g. [".so"]. *)

    val ext_exe : t -> string
    (** [ext_exe] is the file extension for binary executables, e.g. [".exe"]
        or [""]. Until at least OCaml 4.03 this information is not readily
        available (see
        {{:http://caml.inria.fr/mantis/view.php?id=7172}PR #7173}) and
        discovered as described in
        {{:http://lists.ocaml.org/pipermail/wg-windows/2015-July/000037.html}
        this message}. *)

    val native : t -> bool
    (** [native] is [true] if native compilation (i.e. [ocamlopt]) is
        available. Until at least OCaml 4.03 this information is not
        readily available (see
        {{:http://caml.inria.fr/mantis/view.php?id=7172}PR #7173}) and
        [true] is returned iff the standard library directory has the
        [libasmrun] C static library. *)

    val native_dynlink : t -> bool
    (** [native_dynlink] is [true] if native dynamic linking is
        available.  Until at least OCaml 4.03 this information is not
        readily available (see
        {{:http://caml.inria.fr/mantis/view.php?id=7172}PR #7173}) and
        [true] is returned iff the standard library directory has the
        [dynlink.cmxa] library. *)

    val word_size : t -> int
    (** [word_size] is the bit size of one word on the OS that will
        execute the programs produced by the compiler (i.e. the value
        of {!Sys.word_size} in these programs). Until at least OCaml
        4.03 this information is not readily available (see
        {{:http://caml.inria.fr/mantis/view.php?id=7172}PR #7173}) and
        the information is read from the C SIZEOF_PTR macro defined in
        the file [caml/config.h] of the standard library directory. *)

    val dump : Format.formatter -> t -> unit
    (** [dump ppf c] prints an unspecified representation of [c] on [ppf]. *)
  end
end

(** Exts defines sets of file extensions. *)
module Exts : sig

  (** {1:fexts File extensions} *)

  type ext
  (** The type for file extensions. *)

  type t = ext list
  (** The type for lists of file extensions. *)

  val interface : t
  (** [interface] is [exts [".mli"; ".cmi"; ".cmti"]]. *)

  val cmx : ext list
  (** [cmx] is [ext ".cmx"]. *)

  val api : t
  (** [api] is [interface @ cmx]. *)

  val c_library : ext list
  (** [c_library] is the extension for C libraries (archives). The
      actual value is determined from {{!Conf.OCaml}OCaml's configuration}. *)

  val c_dll_library : ext list
  (** [c_dll_library] is the extension for C dynamic libraries (archives). The
      actual value is determined from {{!Conf.OCaml}OCaml's configuration}. *)

  val library : ext list
  (** [library] is [exts [".cma"; ".cmxa"; ".cmxs"] @ c_library] *)

  val module_library : ext list
  (** [module_library] is [(api @ library)]. *)

  val exe : ext list
  (** [exe] is the extension for executables. The actual value is determined
      from {{!Conf.OCaml}OCaml's configuration}. *)

  val exts : string list -> ext list
  (** [exts ss] is [ss] as a list of extensions. *)

  val ext : string -> ext list
  (** [ext s] is [s] as a list of extensions. *)

  (**/**)
  val ext_to_string : Conf.OCaml.t -> ext -> string
  (**/**)
end

(** Package description.

    See the {{!basics}basics}. *)
module Pkg : sig

  (** {1:install Installation description}

      The installation description generates an opam install file
      which is simply a description of file moves (in the [mv] sense)
      from the build or source directory to standard install
      directories. Describing these moves in a given build
      configuration effectively determines what needs to built by the
      {{!build}package build command}.

      {b Note.} Always use ["/"] as a directory seperator for paths, even
      on Windows. *)

  type install
  (** The type for representing a set of install moves. *)

  val nothing : install
  (** [nothing] is an empty set of install moves. *)

  val flatten : install list -> install
  (** [flatten installs] is the union of all the install moves in [installs]. *)

  type field =
    ?force:bool -> ?built:bool -> ?cond:bool -> ?exts:Exts.t -> ?dst:fpath ->
    fpath -> install
  (** The type for an install field, a function that describe file
      moves to a particular installation directory. In the simplest form
      a call [field src] simply moves the file [src] at the root of the
      install directory of the field.

      In general [field ~force ~built ~cond ~exts ~dst src] generates install
      move as follows:
      {ul
      {- [dst] is the path were the source is written to. Expressed
         relative to the install directory of the field. Defaults
         to [Fpath.basename src], i.e. at the root of the install directory.
         If [dst] is a {{!Fpath.is_dir_path}directory path}, the destination
         is [(dst ^ Fpath.basename src)].}
      {- If [exts] is present and non empty, generates the list of
         paths [List.map (fun e -> src ^ e)] and a move for
         each of these. For example [field ~exts:]{!Exts.api}[ "src/m"] would
         generate a move for the files ["src/m.mli"], ["src/m.cmi"],
         ["src/m.cmti"], ["src/m.cmx"].}
      {- If [cond] is [false] (defaults to [true]) no file move is generated.
         This provides a convenient way to conditionalize installation based
         on the build configuration for example:
      {[let jsoo = Conf.value jsoo in
Pkg.mllib ~cond:jsoo "src/mylib_jsoo.mllib"
      ]}}
      {- If [built] is [true] (default), [src] is expressed relative
         to the {{!build}build directory} of the distribution and the path
         [src] is be given to {{!build}build system invocation}
         for construction.
         If [false], [src] is relative to the root of the distribution
         and is excluded from the build system invocation; this can
         be used for installing files that don't need to be built.}
      {- If [force] is [true] (defaults to [false]) it disables the automatic
         [src] filtering performed by the library. When [false],
         the library automatically disable certain build artefact
         depending on {{!Conf.OCaml}OCaml's configuration}, one such
         example is filtering native code build artefact if the OCaml install
         doesn't support
         {{!Conf.OCaml.native}native code compilation}}} *)

  type exec_field = ?auto:bool -> field
  (** The type for fields that install executable files. This is like {!field}
      except for the additional [auto] parameter:
      {ul
      {- If [auto] is [true] (default) then [field src dst]
         automatically adds the [".native"] suffix to [src] if
         {!Conf.OCaml.native} is [true] and the [".byte"] suffix
         otherwise. Besides it automatically adds {!Exts.exe} to
         [dst], which handles things correctly on the various Windows
         ports.}
      {- If [auto] is [false] you have full control according to
         {!field}.}} *)

  val bin : exec_field
  (** [bin] is a field that installs to a common [bin/] directory. *)

  val doc : field
  (** [doc] is a field that installs to a package specific [doc/]
      directory *)

  val etc : field
  (** [etc] is a field that installs to a package specific [etc/]
      directory. *)

  val lib : field
  (** [lib] is a field that installs to a package specific [lib/]
      directory. *)

  val lib_root : field
  (** [lib_root] is a field that install to a common [lib/] directory. *)

  val libexec : exec_field
  (** [libexec] is a field that installs to a package specific [lib/]
      directory but with the executable bit set. *)

  val libexec_root : exec_field
  (** [libexec_root] is a field that install to a common [lib/] directory
      but with the executable bit set. *)

  val man : field
  (** [man] is a field that installs to a common [man/] directory. See
      the {{:https://opam.ocaml.org/doc/manual/dev-manual.html#sec25}
      opam manual} for details. *)

  val misc : field
  (** [misc] is a field that installs to an arbitrary absolute path,
      the user is prompted for authorization,
      see the {{:https://opam.ocaml.org/doc/manual/dev-manual.html#sec25}
      opam manual} for details. *)

  val sbin : exec_field
  (** [sbin] is a field that installs to a common [sbin/] directory. *)

  val share : field
  (** [share] is a field that installs to a package specific [share/]
      directory. *)

  val share_root : field
  (** [share_root] is a field that installs to a common [share/]
      directory. *)

  val stublibs : field
  (** [stublibs] is a field that install to a common [lib/stublibs/]
      directory. Used for OCaml C stub directory. *)

  val toplevel : field
  (** [toplevel] is a field that installs to a common [lib/toplevel/]
      directory. *)

(**/**)
  val unknown : string -> field
(**/**)

  (** {2:tests Test executable description} *)

  val test : ?run:bool -> ?dir:fpath -> ?args:Cmd.t -> exec_field
  (** [test] is a special {{!exec_field}executable field}: it doesn't
      install the described executable (as such the [dst] argument is
      ignored at the moment). If [run] is [true] (default) executes
      the test with [args] when [ocaml pkg/pkg.ml test] is run; this
      will notably run to test the distribution tarball. If
      [run] is [false] the test needs to be invoked explicitely.
      [dir] specifies the current working directory for the test, expressed
      relative to the root directory of the package (defaults to [.]). *)

  (** {2 OCamlbuild higher-level installs}

      The following functions are OCamlbuild specific higher level
      installs that generate moves by reading OCamlbuild specification
      files. They also automatically handle the {!Conf.debugger_support}
      configuration key by building and installing the build artefacts
      needed by debuggers whenever its value is [true]. *)

  val mllib :
    ?field:field -> ?cond:bool -> ?cma:bool -> ?cmxa:bool -> ?cmxs:bool ->
    ?api:string list -> ?dst_dir:fpath -> fpath -> install
  (** [mllib ~field ~cond ~cma ~cmxa ~cmxs ~api ~dst_dir mllib] installs an
      OCaml library described by the
      {{:https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc#Sec_Archives_documentation}OCamlbuild .mllib file} [mllib] with:
      {ul
      {- [field] is the field where it gets installed (defaults to {!lib}).}
      {- If [cond] is [false] (defaults to [true]), no move is generated.}
      {- [cma], [cmxa], [cmxs] determine if corresponding archives are
         built and installed, they all default to [true].}
      {- [api] is the list of modules that defines the public interface
         of the library, if [None] all the modules mentioned in [mllib]
         are part of the public interface.}
      {- [dst_dir] is the destination directory of the library
         in the field. If unspecified this is the root of the field's
         directory.}} *)

  val clib :
    ?dllfield:field ->
    ?libfield:field ->
    ?cond:bool -> ?lib_dst_dir:fpath -> fpath -> install
  (** [clib clib] installs C stubs described by the {{:https://github.com/ocaml/ocamlbuild/blob/master/manual/manual.adoc#advanced-targets}OCamlbuild .clib file} [clib] with:
      {ul
      {- [dllfield] is the field where the C DLL archive gets installed,
         (defaults to {!stublibs})}
      {- [libfield] is the field where the C static archive gets installed
         (defaults to {!lib})}
      {- If [cond] is [false] (defaults to [true]), no move is generated.}
      {- [lib_dst_dir] is the destination directory of the library in the
         [libfield] field. If unspecified this is the root of the field's
         directory. This does not affect the [dllfield], DLLs are always
         installed at the root directory of the [dllfield].}} *)

  (** {1:build Build description} *)

  type build
  (** The type for package build description. *)

  val build :
    ?prepare_on_pin:bool ->
    ?dir:fpath ->
    ?pre:(Conf.t -> unit result) ->
    ?cmd:(Conf.t -> Conf.os -> fpath list -> unit result) ->
    ?post:(Conf.t -> unit result) ->
    ?clean:(Conf.os -> build_dir:fpath -> unit result) -> unit -> build
  (** [build ~prepare_on_pin ~dir ~cmd ~pre ~post] describes the package
      build procedure.
      {ul
      {- [prepare_on_pin] if [true] (default) distribution
         preparation is performed if a [`Pin]
         {{!Conf.build_context}build context} is detected. This means that
         the checkout is watermarked and the massage hook is invoked,
         see step 2. of {{!distdetails}distribution creation}.}
      {- [dir] is the directory where build artefacts are generated,
         (defaults to ["_build"]). Note that his value can be overriden
         from the command line.}
      {- [pre] is a hook that is invoked with the build context, after
         distribution preparation if applicable, but before the build
         command. It can be used to adapt the build setup according to
         the build configuration. Default is a nop.}
      {- [cmd] invokes the build system to build the files
         determined by {{!install}install} moves.
         It is given the build configuration, an {{!Conf.os}OS
         specification}, the list of files to build relative to the
         {{!Conf.build_dir}build directory}, and build the given
         files in the build directory. The default is:
{[
fun c os files -> OS.Cmd.run @@ Cmd.(Pkg.build_cmd c os %% of_list files)
]}}
      {- [post] is a hook that is invoked with the build context after
         the build command returned sucessfully. Default is a nop.}
      {- [clean] is invoked to clean a build. It is given
         an {{!Conf.os}OS specification} and a build directory to
         clean. The default is:
{[
let clean os ~build_dir = OS.Cmd.run @@ Pkg.clean_cmd os ~build_dir
]}}}
      {b Warning.} If you are invoking tools in your hooks consider
      using {!Conf.tool} to look them up it helps for cross-compilation. *)

  (** {2:ocamlbuild OCamlbuild support}

      If you are using [ocamlbuild], the following functions
      help to customize the build system invocation according to the
      configuration. *)

  val build_cmd : Conf.t -> Conf.os -> Cmd.t
  (** [build_cmd c os] is the default build command to which
      files to build are given. Its value is defined by:
{[fun c os ->
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  let toolchain =
    match Topkg_conf.toolchain c with
    | Some toolchain -> Topkg_cmd.(v "-toolchain" % toolchain)
    | _ -> Topkg_cmd.empty
  in
  let debug = Cmd.(on (Conf.debug c) (v "-tag" % "debug")) in
  let profile = Cmd.(on (Conf.profile c) (v "-tag" % "profile")) in
  Cmd.(ocamlbuild % "-use-ocamlfind" %% toolchain % "-classic-display" %%
                    debug %% profile % "-build-dir" % build_dir)]} *)

  val clean_cmd : Conf.os -> build_dir:fpath -> Cmd.t
  (** [clean_cmd os ~build_dir] is the default clean command. Its value
      is defined by:
{[fun os ~build_dir ->
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  Cmd.(ocamlbuild % "-use-ocamlfind" % "-classic-display" %
                    "-build-dir" % build_dir % "-clean") ]} *)

  val ocb_tag : Conf.t -> 'a Conf.key -> string -> Cmd.t
  (** [ocb_tag c key name] is a command fragment adding the
      [ocamlbuild] parameterized tag [name] with [key]'s value to
      the default set of tags. The key value is converted to a string
      using the printer of the key value {{!Conf.conv}converter}.

      For example with a key [k : bool Conf.key] whose value is
      [true], [ocb_tag c k "foo"] adds the tag [foo(true)] to the
      default tags. A sample build command for {!build} using
      this key would be:
{[
  let cmd c os files =
    OS.Cmd.run Cmd.(build_cmd c os %% ocb_tag c k "foo" %% of_list files)]} *)

  val ocb_bool_tag : Conf.t -> bool Conf.key -> string -> Cmd.t
  (** [ocb_bool_tag c key name] is a command fragment adding
      the [ocamlbuild] tag [name] to the default set of tags iff [key]'s
      value is [true]. *)

  val ocb_bool_tags : Conf.t -> (bool Conf.key * string) list -> Cmd.t
  (** [ocb_bool_tags c assoc] is the concatenation of {!ocb_bool_tag}
      for all pairs in [assoc]. *)

  (** {1:distrib Distribution description} *)

  type watermark = string * [ `String of string | `Version | `Version_num
                            | `Name | `Vcs of [`Commit_id]
                            | `Opam of fpath option * string * string]
  (** The type for watermarks. A watermark identifier, e.g. ["ID"] and its
      definition:
      {ul
      {- [`String s], [s] is the definition.}
      {- [`Name], is the name of package.}
      {- [`Version], is the version of the distribution.}
      {- [`Version_num], is the version of the distribution with a potential
         leading ['v'] or ['V'] dropped.}
      {- [`Vcs `Commit_id], is the commit identifier (hash) of the
         distribution. May be post-fixed by ["dirty"] in
         {{!Conf.build_context}dev package ([`Pin]) builds}.}
      {- [`Opam (file, field, sep)], is the values of the field
         [field] concatenated with separator [sep] of the opam file
         [file], expressed relative to the distribution root directory, if
         [file] is [None] this is the package's default opam file, see
         {!describe}. Not all fields are supported see the value of
         {!Topkg_care.Opam.File.field_names}.  {b Warning.} In
         {{!Conf.build_context}dev package ([`Pin]) builds}, [`Opam]
         watermarks are only substituted if the package [topkg-care] is
         installed.}}

      When a file is watermarked with an identifier ["ID"], any occurence of
      the sequence [%%ID%%] in its content is substituted by its definition. *)

  type distrib
  (** The type for describing distribution creation. *)

  val distrib :
    ?watermarks:watermark list ->
    ?files_to_watermark:(unit -> fpath list result) ->
    ?massage:(unit -> unit result) ->
    ?exclude_paths:(unit -> fpath list result) ->
    ?uri:string -> unit -> distrib
  (** [distrib ~watermarks ~files_to_watermark ~massage
      ~exclude_paths ~uri ()] influences the distribution creation
      process performed by the [topkg] tool.
      See the {{!distdetails}full details about distribution creation}.

      In the following the {e distribution build directory} is a
      private clone of the package's source repository's [HEAD] when
      [topkg distrib] is invoked.
      {ul
      {- [watermarks] defines the source watermarks for the distribution,
         defaults to {!watermarks}.}
      {- [files_to_watermark] is invoked in the distribution build
         directory to determine the files to watermark, defaults
         to {!files_to_watermark}.}
      {- [massage] is invoked in the distribution build directory,
         after watermarking, but before archiving. It can be used to
         generate distribution time build artefacts. Defaults to {!massage}.}
      {- [exclude_paths ()] is invoked in the distribution build
         directory, after massaging, to determine the paths that are
         excluded from being added to the distribution archive. Defaults to
         {!exclude_paths}.}
      {- [uri] is an URI pattern that specifies the location of the
         distribution on the WWW. In this string any sub-string
         ["$(NAME)"] is replaced by the package name, ["$(VERSION)"] is replaced
         by the distribution version string and ["$(VERSION_NUM)"] by the
         distribution version string, chopping an initial
         ['v'] or ['V'] character if present. This argument is used to
         generate the [url] file of an opam package for the distribution;
         it will be deprecated in the future in favour of a [x-distrib-uri]
         field in the opam file. If the value is unspecified it defaults to:
{[PKG_HOMEPAGE/releases/$(NAME)-$(VERSION_NUM).tbz]}
         where PKG_HOMEPAGE is the package's opam file [homepage] field.
         As a special case if the
         hostname of PKG_HOMEPAGE is [github] the following is used:
{[PKG_DEV_REPO/releases/download/$(VERSION)/$(NAME)-$(VERSION_NUM).tbz]}
         where PKG_DEV_REPO is the package's opam file [dev-repo] field
         without the [.git] suffix and a possible [git+] prefix.}} *)

  val watermarks : watermark list
  (** [watermarks] is the default list of watermarks. It has the following
      elements:
      {ul
      {- [("NAME", `Name)]}
      {- [("VERSION", `Version)]}
      {- [("VERSION_NUM", `Version_num)]}
      {- [("VCS_COMMIT_ID", `Vcs [`Commit_id])]}
      {- [("PKG_MAINTAINER", `Opam (None, "maintainer", ", "))]}
      {- [("PKG_AUTHORS", `Opam (None, "authors", ", ")]}
      {- [("PKG_HOMEPAGE", `Opam (None, "homepage", " ")]}
      {- [("PKG_ISSUES", `Opam (None, "bug-reports", " ")]}
      {- [("PKG_DOC", `Opam (None, "doc", " "))]}
      {- [("PKG_LICENSE", `Opam (None, "license", ", ")]}
      {- [("PKG_REPO", `Opam (None, "dev-repo", " "))]}}
      Prepending to the list overrides default definitions. *)

  val files_to_watermark : unit -> fpath list result
  (** [files_to_watermark ()] is the default list of files to
      watermark.  It is invoked in the distribution build directory
      and gets the set of {{!Vcs.tracked_files}tracked files} of this
      directory from which it removes the files that end with [.flv],
      [.gif], [.ico], [.jpeg], [.jpg], [.mov], [.mp3], [.mp4], [.otf],
      [.pdf], [.png], [.ttf], [.woff]. *)

  val massage : unit -> unit result
  (** [massage] is the default distribution massaging function. It is
      invoked in the distribution build directory and does nothing. *)

  val exclude_paths : unit -> fpath list result
  (** [exclude_paths ()] is the default list of paths to exclude
      from the distribution archive. It is invoked in the distribution build
      directory and returns the following static set of files.
{[
fun () -> Ok [".git"; ".gitignore"; ".gitattributes"; ".hg"; ".hgignore";
              "build"; "Makefile"; "_build"]]} *)

  (** {1 Distribution publication description} *)

  type publish
  (** The type for describing distribution publication. *)

  val publish :
    ?artefacts:[`Doc | `Distrib | `Alt of string ] list -> unit -> publish
  (** [publish ~artefacts ()] influences the distribution publication process
      performed by the [topkg] tool:
      {ul
      {- [artefacts] defines which artefacts are published by an invocation
         of [topkg publish] without arguments (defaults to
         [[`Doc;`Distrib]]).}} *)

  (** {1 Package description} *)

  type std_file
  (** The type for specifying a standard file. *)

  val std_file : ?install:bool -> fpath -> std_file
  (** [std_file ~install p] is a standard file [p] expressed relative
      to the distribution root directory. The file is
      automatically installed if [install] is [true] (default). *)

  type meta_file
  (** The type for specifying an OCamlfind META file. *)

  val meta_file : ?lint:bool -> ?install:bool -> fpath -> meta_file
  (** [meta_file ~lint ~install p] is a META file [p] expressed relative
      to the distribution root directory. The file is automatically
      installed in the {!lib} field if [install] is [true] (default).
      If [lint] is [true] (default), it is OCamlfind linted. *)

  type opam_file
  (** The type for specifying an opam file. *)

  val opam_file :
    ?lint:bool -> ?lint_deps_excluding:string list option -> ?install:bool ->
    fpath -> opam_file
  (** [opam_file ~lint ~lint_deps_excluding ~install p] is an opam file
      [p] expressd relative to the distribution root directory such that:
      {ul
      {- If [install] is [true] (default), it is automatically installed
         in the {!lib} field.}
      {- If [lint] is [true] (default), it is opam linted.}
      {- If [lint_deps_excluding] is [Some excludes], [topkg]
         checks that each of the opam package dependencies is mentioned
         as a root package in the OCamlbuild [_tags] file and vice-versa. The
         following package names are excluded from this test:
         {ul
         {- The packages names mentioned in [excludes].}
         {- Package names that start with ["conf-"]}
         {- {!Topkg_care.OCamlfind.base_packages}}
         {- {!Topkg_care.Opam.ocaml_base_packages}}}
         If [None] the dependency check is disabled.}} *)

  val describe :
    ?delegate:Cmd.t ->
    ?readmes:std_file list ->
    ?licenses:std_file list ->
    ?change_logs:std_file list->
    ?metas:meta_file list ->
    ?opams:opam_file list ->
    ?lint_files:fpath list option ->
    ?lint_custom:(unit -> R.msg result list) ->
    ?distrib:distrib ->
    ?publish:publish ->
    ?build:build ->
    string -> (Conf.t -> install list result) -> unit
  (** [describe name install] describes a package named [name] with:
      {ul
      {- [delegate], the package delegate command to use. If unspecfied
         determined by the delegate lookup procedure, see
         [topkg help delegate] for more information.}
      {- [readmes] are readme files, defaults to
         [[std_file "README.md"]].  Automatic install is in the
         {!doc} field.}
      {- [licenses] are license files, defaults to
         [[std_file "LICENSE.md"]].  Automatic install is in the {!doc} field.}
      {- [change_logs] are change logs, defaults to
         [[std_file "CHANGES.md"]]. The first file of the list is the
         one that is acted upon by the [topkg log] command.
         Automatic install is in the {!doc} field.}
      {- [metas] the package's ocamlfind META files, defaults to
         [[ meta_file "pkg/META" ]].}
      {- [opams] the package's opam package files, defaults to
         [[opam_file "opam"]]. The default opam file used by a package
         description depends on the package [name] (which can
         be overriden from the command line). The opam file lookup
         procedure selects the first path in [opams] whose filename is
         [(name ^ ".opam")] and, failing
         to do so, it fallbacks to an ["opam"] file at the root of the
         distribution.}
      {- [lint_files] if [Some files], ensures that all files mentioned in
         [readme], [license], [change_log], [metas], [opams] and [files]
         are present in the distribution. Defaults to [Some []].
         If [None] disables the file existence tests (including readme,
         change_log, license, metas, opams, metas.)}
      {- [lint_custom] defines a custom linting process run with the current
         directory set at the root of the distribution. Successes and errors
         in the returned list are reported as such and any error in the list
         makes the lint fail. Defaults to [None].}
      {- [distrib], specifies the distribution process, defaults to
         {!distrib}[ ()].}
      {- [publish], specifies the publication process, defaults to
         {!publish}[ ()].}
      {- [build], specifies the build process, defaults to {!build}[ ()].}
      {- [install] given a {{!Conf.t}build configuration} specifies
         the install moves. Note that some of standard files are
         automatically installed and don't need to be specified, see
         {!std_file}, {!meta_file} and {!opam_file}.}} *)

  (** {1:distdetails Package distribution creation details}

      The following describes the exact steps performed by [topkg
      distrib] to create the distribution archive. Note that [topkg]
      allows to override or disable part of the process via command
      line arguments, e.g. to specify the version string manually or
      skip linting. See [topkg distrib --help] for more information.

      The distribution process assumes that the source repository
      working directory is clean so that its definitions are consistent
      with those of the distribution build directory. A warning is
      generated if this is not the case as it may end up in inconsistent
      distribution archives (but which may be fine to only publish
      a documentation update).

      Let [$NAME] be the name of the package, [$BUILD] be its
      {{!build}build directory}, [$VERSION] be the VCS tag description
      (e.g.  [git-describe(1)] if you are using [git]) of the source
      repository HEAD commit and [distrib] the {{!distrib}distribution
      description} found in the source's repository [pkg/pkg.ml] file.
      {ol
      {- Clone the source repository at [HEAD] as the distribution build
         directory [$BUILD/$NAME-$VERSION.build].}
      {- Prepare the distribution:
        {ol
         {- Invoke the [files_to_watermark] function of [distrib] in the
            distribution build directory to determine the files to watermark
            with [watermarks] and perform the watermarking process.}
         {- Adds a version field with value [$VERSION] to the opam files
            mentioned by {!Pkg.describe}.}
         {- Run the [massage] function of [distrib] in the distribution
            build directory. This can be used to create distribution time
            build artefacts.}}}
      {- Invoke the [exclude_paths] function of [distrib] in the
         distribution build directory to determine the paths to exclude
         from the archive.}
      {- Create a distribution tarball [$BUILD/$NAME-$VERSION.tbz] with the
         file hierarchy in [$BUILD/$NAME-$VERSION.build],
         excluding the paths determined at the preceeding point and delete the
         clone [$BUILD/$NAME-$VERSION.build]. File modifications times in
         the archive are set to [HEAD]'s commit time and file
         permissions are preserved. Any other form of file metadata is
         discarded in the archive.}
      {- Test the distribution. Unpack it in directory [$BUILD/$NAME-$VERSION],
         lint the distribution, build the package in the current
         build environment with its tests, run the tests, on success
         delete [$BUILD/$NAME-$VERSION]. Note that this uses the archive's
         [pkg/pkg.ml] file, which should not be different from the source's
         repository file if the latter was clean when [topkg distrib] was
         invoked.}}

      {2:watnote Note on watermarking}

      It is right to doubt the beauty and be concerned about the
      watermarking process. However experience shows that alternatives
      like having an OCaml module generated with the appropriate
      information doesn't work well in practice. Version numbers do
      not only show up in OCaml source code. They also appear in
      documentation comments, metadata files, textual data files and
      non-OCaml source files.

      Watermarking by default all the non binary files of the
      distribution allows one to write %‌%VERSION%% in any context and
      be sure it is be substituted with the right version number in
      dev package ([`Pin]) and distribution ([`Distrib])
      {{!Conf.build_context}build contexts} (this occurence was not
      subsituted because a ZERO WIDTH NON-JOINER U+200C was introduced between
      the first two percent characters).

      If this scheme poses a problem for certain files or you remain
      unconvinced, simply filter the result of {!files_to_watermark} or
      replace it by the exact files you would like to watermark.  *)
end

(** {1:private Private} *)

(** Private definitions.

    {b Warning.} The following definitions are subject to change even
    between minor versions of the library. [Topkg] users {b must not}
    use these definitions to describe their package. *)
module Private : sig

  (** {1:private Private} *)

  val disable_main : unit -> unit
  (** [disable_main ()] disables [Topkg]'s main invoked on
      {!Pkg.describe}. Invoke this function in your main function if
      you are not using [Topkg] in a description file but as as a
      library. *)

  (** Topkg interprocess communication codec.

      Codecs for communication between the [topkg] tool and topkg
      description files. *)
  module Codec : sig

    (** {1 Decode errors} *)

    (** The type for decode errors.
        {ul
        {- [Corrupted (kind, data)], an error occured while decoding
           [data] for [kind].}
        {- [Version (exp, fnd)], a {{!version}versioned} decoder
           expected version [exp] but found [fnd]}} *)
    type error = Corrupted of (string * string) | Version of int * int

    val pp_error : Format.formatter -> error -> unit
    (** [pp_error ppf e] prints an unspecified representation of [e]
        on [ppf]. *)

    exception Error of error
    (** Raised on decode errors. *)

    (** {1:codecs Codecs} *)

    type 'a t
    (** The type for codec for OCaml values of type ['a]. *)

    val v : kind:string -> enc:('a -> string) -> dec:(string -> 'a) -> 'a t
    (** [v kind enc dec] is a codec for value identified as [kind] using
        [enc] to encode and [dec] to decode. *)

    val kind : 'a t -> string
    (** [kind c] is [c]'s kind. *)

    val enc : 'a t -> 'a -> string
    (** [enc c] is [c]'s encoder. *)

    val dec : 'a t -> string -> 'a
    (** [dec c] is [c]'s decoder. The decoder @raise Error in case of
        decode error *)

    val dec_result : 'a t -> string -> 'a result
    (** [dec c] is like {!dec} but doesn't raise. The exception is
        turned into an error message using {!pp_error}. *)

    val with_kind : string -> 'a t -> 'a t
    (** [with_kind k c] is [c] with kind [k]. *)

    val write : fpath -> 'a t -> 'a -> unit result
    (** [write f c v] encodes value [v] with [c] to [f]. *)

    val read : fpath -> 'a t -> 'a result
    (** [read f c] reads a value with [c] from [f]. *)

    (** {1:base Base type codecs} *)

    val unit : unit t
    (** [unit] codecs a [()]. *)

    val const : 'a -> 'a t
    (** [const v] codecs the constant [v]. *)

    val bool : bool t
    (** [bool] codecs booleans. *)

    val int : int t
    (** [int] codecs integers. *)

    val string : string t
    (** [string] codecs strings. *)

    val option : 'a t -> 'a option t
    (** [option el] codecs [el] options. *)

    val result : ok:'a t -> error:'b t -> ('a, 'b) Result.result t
    (** [result ~ok ~error] codecs [ok], [error] results. *)

    val list : 'a t -> 'a list t
    (** [list el] codecs [el] lists. *)

    val pair : 'a t -> 'b t -> ('a * 'b) t
    (** [pair c0 c1] codecs [c0], [c1] pairs. *)

    val t3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
    (** [t3] is like {!pair} but for triples. *)

    val t4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
    (** [t4] is like {!pair} but for quadruples. *)

    val t5 : 'a t -> 'b t -> 'c t -> 'd t -> 'e t ->
      ('a * 'b * 'c * 'd * 'e) t
    (** [t5] is like {!pair} but for qintuples. *)

    val alt : kind:string -> ('a -> int) -> 'a t array -> 'a t
    (** [alt tag cs] codecs values by tagging them with [tag] and
        using the corresponding codec in [cs].

        @raise Invalid_argument if [Array.length cs > 256]. *)

    val version : int -> 'a t -> 'a t
    (** [version num c] versions codec [c] with number [num].
        On decode a version number mismatch raises an error
        see {!error}. *)

    val view : ?kind:string -> ('a -> 'b) * ('b -> 'a) -> 'b t -> 'a t
    (** [view kind t c] views [t] as [c] for codecing. *)

    (** {1:topkg Topkg types} *)

    val msg : [`Msg of string ] t
    (** [msg] codecs error messages. *)

    val result_error_msg : 'a t -> 'a result t
    (** [result_error_msg ok] codecs [ok] or error message results. *)

    val fpath : Fpath.t t
    (** [fpath] codecs files paths. *)

    val cmd : Cmd.t t
    (** [cmd] codecs command line fragments. *)
  end

  (** Package description. *)
  module Pkg : sig

    type t
    (** The type for package descriptions. *)

    val empty : t
    (** [empty] is an empty package description. *)

    val name : t -> string
    (** [name p] is [p]'s name. *)

    val delegate : t -> Cmd.t option
    (** [delegate p]is [p]'s delegate. *)

    val build_dir : t -> fpath
    (** [build_dir p] is [p]'s build directory. *)

    val readmes : t -> fpath list
    (** [readme p] is [p]'s readme files. *)

    val change_logs : t -> fpath list
    (** [change_logs p] is [p]'s change logs. *)

    val licenses : t -> fpath list
    (** [licenses p] is [p]'s license files. *)

    val opam : name:string -> t -> fpath
    (** [opam name p] is [p]'s opam file for opam package [name]. *)

    (** {1:distrib Distrib} *)

    val distrib_uri : t -> string option
    (** [distrib_uri p] is [p]'s distribution location URI pattern.
        See {!Pkg.distrib}. *)

    (** {1:publish Publish} *)

    val publish_artefacts : t -> [`Distrib | `Doc | `Alt of string ] list
    (** [publish_artefacts p] is [p]'s distribution publication artefacts.
        See {!Pkg.publish}. *)

    (** {1:lints Lints}

        {b Note.} In the following [None] values mean that
        the lint is disabled by the package description. *)

    val lint_custom : t -> (unit -> R.msg result list) option
    (** [lint_custom p] is [p]'s custom linting function (if any).

        {b Note.} Use {!Ipc.lint_custom} to run the function
        from another program. *)

    val lint_files : t -> fpath list option
    (** [lint_files p] are [p]'s files to check for existence. *)

    val lint_metas : t -> (fpath * bool) list
    (** [lint_metas p] are [p]'s META file to OCamlfind lint. *)

    val lint_opams : t -> (fpath * bool * string list option) list
    (** [lint_opams p] are [p]'s opam file opam lint and dependency
        lint. *)

    (** {1:codec Codec} *)

    val codec : t Codec.t
    (** [codec] is a codec for package descriptions. *)
  end

  (** Topkg interprocess communication. *)
  module Ipc : sig

    (** {1:ipc Interprocess communication} *)

    type 'a t
    (** The type for interpocess communication transfering values of
        type ['a]. *)

    val cmd : 'a t -> Cmd.t
    (** [cmd ipc] are the command line arguments provided to the child
        process. *)

    val codec : 'a t -> 'a Codec.t
    (** [codec ipc] is the codec used to transfer the value. *)

    val answer : 'a t -> fpath
    (** [answer ipc] is the file path from which the value can
        be decoded from. *)

    (** {1:req Requests} *)

    val pkg : unit -> Pkg.t t
    (** [pkg ()] is an IPC to get the package description. *)

    val lint_custom : unit -> R.msg result list option t
    (** [lint_custom ()] is an IPC to run the custom linting. *)

    val distrib_prepare :
      dist_build_dir:fpath -> name:string -> version:string -> opam:fpath ->
      fpath list result t
    (** [distrib_prepare dist_build_dir name version opam] is an IPC to
        prepare a distribution in directory [dist_build_dir]. This
        sets the cwd to [dist_build_dir], performs the distribution
        watermarking process with [name] used for [`Name], [version] used
        for [`Version] and [opam] as the default file for opam watermarks.
        It then performs distribution massaging and returns the file paths
        to exclude from the distribution archive. *)
  end

  (** opam helpers. *)
  module Opam : sig

    (** {1:opam opam} *)

    (** opam package file access.

        Normally opam metadata access is only needed at distribution
        time and this is handled by {!Topkg_care.Opam.File} using the
        [opam-format] library.

        However there is one case where we want to be able to access
        the metadata from [Topkg]: on pin builds where the
        {{!Pkg.watermark}watermarking} process needs to be run to turn
        the repo into a pseudo-distribution.

        Since we don't want [Topkg] to have any dependency and that
        [opam] currently doesn't allow to consult the fields of arbitrary
        opam files (see
        {{:https://github.com/ocaml/opam/issues/2446} issue #2446}) we
        assume a pin build has the [topkg] tool installed and call
        to it to get the opam fields for watermarking (if [topkg] is
        unavailable the watermarks are simply undefined). *)
    module File : sig

      (** {1:file opam file} *)

      type t = (string * string list) list
      (** The type for a simplified model the fields of an opam
          file. See {!Topkg_care.Opam.File}. *)

      val codec : t Codec.t
      (** [codec] is a codec for opam file fields. *)

      val fields : fpath -> ((string * string list) list) result
      (** [fields file] are the fields of the opam file [file] which
          are obtained by calling the [topkg] topkg executable. *)
    end
  end
end

(** {1:basics Basics}

{!Topkg} is a packager for distributing OCaml software. Fundamentally
it only provides a simple and flexible mechanism to describe an opam
{{:http://opam.ocaml.org/doc/manual/dev-manual.html#sec25}[install]
file} according to the build configuration which is used to infer one
corresponding invocation of your build system.

This simple idea brings the following advantages:
{ol
{- It frees you from implementing an install procedure in your build
   system: this task is delegated to {{:http://opam.ocaml.org}opam},
   to the [opam-installer] tool or anything that understands an opam
   install file.}
{- It doesn't reclaim control over your build system. It only invokes
   it {e once} with a list of targets determined by the package
   {{!section:Pkg.install}install description}.}
{- It is very flexible, supports a wide range of installation scenarios
   and is expressed in the reasonable language.}}

Beyond this a [Topkg] package description provides information about
the package's distribution creation and publication procedures. This
enables swift and correct package distribution management via the [topkg]
tool, see {!care}.

{ul
{- {!setup}}
{- {!build}}
{- {!descr}}
{- {!installdescr}}
{- {!care}}
{- {!advanced}
   {ul {- {!config_store}}
       {- {!multiopam}}}}
{- {!menagerie}}}

{1:setup Source repository setup}

The root of you source repository should have the following layout:
{ul
{- [pkg/pkg.ml], the package description written with {!Topkg}'s API.
   See {{!descr}package description.}}
{- [pkg/META], an
    {{:http://projects.camlcity.org/projects/findlib.html}ocamlfind}
    META file describing your package. See {!Pkg.describe} to configure this.}
{- [_tags], ocamlbuild file with at least a [true : bin_annot] line.
   See {{!cmt}handling cmt and cmti} files for details.}
{- [opam], the package's opam metadata. See {!build}.}
{- [README.md], your readme file. See {!Pkg.describe} to configure this.}
{- [LICENSE.md], your license file. See {!Pkg.describe} to configure this.}
{- [CHANGES.md], your change log. See {!Pkg.describe} to configure this.}}

{2:carcass_ad Quick setup (advertisement)}

If you start a new library
{{:http://erratique.ch/software/carcass}[carcass]} can generate
the structural boilerplate with your personal information and preferences.
Invoke:
{v
carcass body topkg/pkg mypkg
(cd mypkg && git init && git add . && git commit -m "First commit.")
opam pin add -kgit mypkg mypkg#master
v}

and you have a library that is [{opam,ocamlfind}]-able with correct
version watermarks on releases and opam pins. You are now only a few
invocations away to release it in the OCaml opam repository, see
[topkg help release] for doing so; but don't forget to document it and
make it do something useful.

{1:build opam and package build instructions}

The package needs to build-depend on [topkg] as well as [ocamlfind]
which is used by the package description file [pkg/pkg.ml] to find the
[topkg] library; it is likely that you are using [ocamlbuild] too. So
the depends field of your opam file should at least have:

{v
depends: [
  "ocamlfind" {build}
  "ocamlbuild" {build}
  "topkg" {build & >= 0.9.0} ]
v}

The build instructions of the package are simply an invocation of
[pkg/pkg.ml] with the [build] command and a specification of the
build configuration on the command line. In the simplest case, if
your package has no configuration options, this simply boils
down to:
{[
# For opam >= 2.0
build: [[ "ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%" ]]

# For opam < 2.0
build: [[ "ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{pinned}%" ]]
]}

The ["--dev-pkg"] configuration key is used to inform the package
description about the {{!Conf.build_context}build context}. This
invocation of [pkg/pkg.ml] executes your build system with a set of
targets determined from the build configuration and generates in the
root directory of your distribution an opam [install] file that opam
uses to install and uninstall your package.

This is all you need to specify. Do not put anything in the remove
field of the opam file. Likewise there is no need to invoke
[ocamlfind] with your [META] file. Your [META] file should simply be
installed in the directory of the [lib] field which happens
automatically by default.

If you described {{!Pkg.tests}tests} then you should specify
the instructions as follows (unfortunately for opam < 2.0 this involves
the repetition of the build line):
{[
# For opam >= 2.0

build:
[[ "ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
                                "--tests" "%{build-test}%" ]]
run-test:
[[ "ocaml" "pkg/pkg.ml" "test" ]]

# For opam < 2.0

build:
[[ "ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{pinned}%" ]]

build-test:
[[ "ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{pinned}%" "--tests" "true" ]
 [ "ocaml" "pkg/pkg.ml" "test" ]]
]}

{b Beyond opam.} If you need to support another package system you
can invoke [pkg/pkg.ml] as above and then manage the installation and
uninstallation at a given [$DESTDIR] with the generated opam [install]
file using [opam-installer] tool or any other program that understand
these files.

{1:descr Package description}

The {!Pkg.describe} function has a daunting number of arguments and
configuration options. However if you keep things simple and stick to
the defaults, much of this does not need to be specified.

For example, if we consider the basic {{!setup}setup} mentioned
above for a library described with an OCamlbuild [src/mylib.mllib]
file, your package description file [pkg/pkg.ml] simply looks like
this:
{[
#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mylib" @@ fun c ->
  Ok [ Pkg.mllib "src/mylib.mllib" ]
]}

{b Tip.} To allow
{{:https://github.com/the-lambda-church/merlin}merlin} to function
correctly in your package description, issue [M-x merlin-use topkg] in
[emacs] or [:MerlinUse topkg] in [vim].

This simple description builds and installs the library described by
the [src/mylib.mllib] file. It works correctly if native code
compilation or native dynamic linking is not available. It
automatically installs the package's META file in the directory of the
[lib] field and your readme, change log and license file in the
directory of the [doc] field.

Try to test the package build description with [topkg build], this
builds the package according to the description and build
configuration and writes a [mylib.install] at the root of the
distribution that describes the package installation. If everything
went well, you are now ready to release, see [topkg help release] for
the procedure.

To debug package descriptions it useful to dry run the build. This
prevents the package from building and only writes the [mylib.install] file
determined according to the build configuration.
{[
topkg build -d    # Only write the opam install file
topkg build -d -v # Also print the build configuration
topkg help troubleshoot # More troubleshooting tips
]}
Note that [topkg build] does nothing more than invoke
[ocaml "pkg/pkg.ml" build].  If you would like to see the build
{{!section:Conf.key}configuration
options} of a package description you should do:
{v
ocaml pkg/pkg.ml help
./pkg/pkg.ml help     # If has exec right
v}

{1:installdescr Install description}

An opam [install] file is a description of a standard UNIX install. It
has fields for each of the standard directories [lib], [bin], [man],
[etc], etc. Each of these fields lists the files to install in the
corresponding directory (or subdirectories). See the
{{:http://opam.ocaml.org/doc/manual/dev-manual.html#sec25}install file
specification} in the opam developer manual for more information.

A topkg install description is just a convenient and compact way to
describe an opam install file according to the build configuration. In
turn this also describes what needs to be built which allows topkg to
call the build system appropriately.

For each opam install field there is a corresponding field function
that you can use to generate install moves. The documentation of
{!Pkg.field} and {!Pkg.exec_field} describes how you can use or omit
their various arguments to simplify the description. Topkg also provides
a few higher-level convenience functions like {!Pkg.mllib} and
{!Pkg.clib} which allow to reuse the description work already done
for OCamlbuild.

In the following we review a few basic install use cases. The
{{!menagerie}menagerie} provides links to full and more complex examples.

{2:installlib Installing libraries and C stubs}

It is possible to use the {!Pkg.lib} field function and appropriate
{{!Exts}file extensions} to manually install a library, but this
quickly becomes tedious. The higher-level {!Pkg.mllib} install function
brings this to a single line by reading from a OCamlbuild [mllib] file.
Given a library described in [src/mylib.mllib] file use:
{[
Pkg.mllib "src/mylib.mllib"
]}
This will make all the modules mentioned in the [mllib] file part of
the API (i.e. install its [cmi] files). You can restrict
the API by using the [~api] optional argument. In the following, only
[Mod1] and [Mod2] define the API, the other modules of [mllib] remain hidden
to the end user (but unfortunately not to the linkers...):
{[
Pkg.mllib ~api:["Mod1"; "Mod2"] "src/myllib.mllib"
]}

A shortcut also exists for installing C stubs: {!Pkg.clib}. Simply use
it on an existing [src/libmystub.clib] file (N.B. OCamlbuild
mandates that your [clib] file name starts with [lib]):
{[
Pkg.clib "src/libmystub.clib"
]}
This will generate the appropriate install moves in the [lib] and [stublib]
fields.

{2:installbin Installing binaries}

In opam, binaries can only be installed by using the [bin], [sbin] and
[libexec] fields. Trying to install a binary in other fields will not
set its executable bit
({{:https://github.com/ocaml/opam/issues/2430}discussion}).

Most of the time one wants to install native binaries if native code
compilation is available and bytecode ones if it is not. The [auto]
argument of {!Pkg.exec_field} does exactly this if omited.

So if you have an executable to install in the [bin] field whose [main]
is in [src/myexec.ml], describe it with:
{[
Pkg.bin "src/myexec"
]}
As with any other field it easy to rename the executable along the
way with the [dst] argument:
{[
Pkg.bin "src/myexec" ~dst:"my-exec"
]}
Note that using the default value of [auto] also automatically handles
the extension business for Windows platforms.

{2:installcond Conditional install}

An easy and readable way to handle conditional installs is to use the
[cond] argument of {{!Pkg.field}field functions}. The following shows
how to conditionaly build and install a binary depending on the opam
[cmdliner] package being installed:
{[
let cmdliner = Conf.with_pkg "cmdliner"
let () =
  Pkg.describe "mypkg" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.bin ~cond:cmdliner "src/myexec" ]
]}
Note that {{!Conf.key}configuration keys} must be created before the
call to {!Pkg.describe}. Their value can then be accessed with the
{!Conf.value} from the configuration given to the install move
function you specify.

For this conditional install the opam build instructions look like this:
{[
depopts: [ "cmdliner" ]
build: [[
  "ocaml" "pkg/pkg.ml" "build"
          "--dev-pkg" "%{dev}%" # use "%{pinned}%" for opam < 2.0
          "--with-cmdliner" cmdliner:installed ]]
]}

{2:installautoconds Automatic install conditions}

Some conditions related to native code and native code dynamic linking
happen automatically. For example moves with paths ending with [.cmxs]
are automatically dropped if {!Conf.OCaml.native_dynlink} is [false]
in the current build configuration. This behaviour can be disabled by
using the [force] argument of {{!Pkg.field}field functions}.

{2:installexts Extension sets and platform dependent extensions}

The {{!Pkg.field}field functions} have an optional [exts] argument. If
present these extensions are appended to the [src] path given to the
function. The module {!Exts} defines a few predefined extension
sets. For example a single module library archive implemented in
[src/mylib.ml] can be declared by:
{[
Pkg.lib ~exts:Exts.module_library "src/mylib"
]}
which is, effectively, a shortcut for:
{[
[ Pkg.lib "src/mylib.mli";
  Pkg.lib "src/mylib.cmti";
  Pkg.lib "src/mylib.cmi";
  Pkg.lib "src/mylib.cmx";
  Pkg.lib "src/mylib.cma";
  Pkg.lib "src/mylib.a";
  Pkg.lib "src/mylib.cmxa";
  Pkg.lib "src/mylib.cmxs"; ]
]}

Extensions sets are also used to support platform independent installs.
For example to install a static C library archive you should use
the second invocation, not the first one:
{[
Pkg.lib "src/libmylib.a" (* DON'T do this *)
Pkg.lib ~exts:Exts.c_library "src/libmylib" (* Do this *)
]}
this ensures that the correct, platform dependent, suffix is used.

{2:installrename Renaming and installing in subdirectories}

By default install moves simply install the file at the root directory
of the field. Using the [dst] optional argument you can rename the file and/or
install it to subdirectories.
{[
Pkg.lib "src/b.cmo" ~dst:"hey"  (* Install as [hey] file. *)
Pkg.lib "src/b.cmo" ~dst:"hey/" (* Install in [hey] subdirectory *)
Pkg.lib "src/b.cmo" ~dst:"hey/ho.cmo"  (* Install as [ho.cmo] in [hey]. *)
]}

{2:cmt Handling cmt and cmti files}

Since the OCaml tools generate [.cmt] and [.cmti] files only as a side
effect they are treated specially: they are not built. For
OCamlbuild you should add this line to your [_tags] file:
{[
true : bin_annot
]}
this will build them as a side effect of other build invocations. In
the generated opam install file the [cmt] and [cmti] files are
prefixed by a ? so that if they are not built the install does not
fail.

{1:care Package care}

Developing and distributing packages implies a lot of mundane but
nevertheless important tasks. The [topkg] tool, guided by information
provided by {!Pkg.describe} helps you with these tasks.

For example [topkg lint] makes sure that the package source repository
or distribution follows a few established (or your) conventions and
that the META and opam files of the package pass their respective
linter.  [topkg distrib] will create watermarked and reproducible
distribution archives (see {!Pkg.distrib}) while [topkg publish] and
[topkg opam] will help publishing them. All this and more is described
with details in [topkg]'s help system, invoke:
{v
topkg help release
topkg help
v}

to get an extended introduction and pointers to these features.

{2:doccare Documentation care}

Topkg provides support to make it easier to write and publish your
package documentation. The [topkg doc -r] command generates and refreshes
renderings of the package documentation while you work on it.

Documentation publication is always derived from a generated
distribution archive (the latter doesn't need to be published of
course). So to push documentation fixes and clarifications simply invoke:

{v
topkg distrib
topkg publish doc
v}

Make sure you include %‌%VERSION%% watermarks in the documentation so
that readers know exactly which version they are reading. By default
this will be substituted by a VCS tag description so it will be
precise even though you may not have properly tagged a new release
for the package.

{1:advanced Advanced topics}

{2:config_store Storing build configuration information in software}

The following sample setup shows how to store build configuration
information in build artefacts.

In this example we store the location of the install's [etc] directory
in the build artefacts. The setup also works seamlessly during
development if build artefacts are invoked from the root directory of
the source repository.

We have the following file layout in our source repository:
{v
etc/mypkg.conf       # Configuration file
src/mypkg_etc.ml     # Module with path to the etc dir
v}

the contents of [src/mypkg_etc.ml] is simply:
{[
(* This file is overwritten by distribution builds. During development
   it refers to the [etc] directory at the root directory of the
   source repository. *)

let dir = "etc"
]}
the value [Mypkg_etc.dir] is used in the sources to refer to the [etc]
directory of the install. In the package description file
[pkg/pkg.ml] we have the following lines:
{[
let (* 1 *) etc_dir =
  let doc = "Use $(docv) as the etc install directory" in
  Conf.(key "etc-dir" fpath ~absent:"etc" ~doc)

let (* 2 *) etc_config c = match Conf.build_context c with
| `Dev -> Ok () (* Do nothing, the repo src/mypkg_etc.ml will do *)
| `Pin | `Distrib ->
    let config = strf "let dir = %S" (Conf.value c etc_dir) in
    OS.File.write "src/mypkg_etc.ml" config

let () =
  let build = Pkg.build ~pre:etc_config () in
  Pkg.describe "mypkg" ~build @@ fun c ->
  Ok [ (* 3 *) Pkg.etc "etc/mpypkg.conf"; ... ]
]}

In words:
{ol
{- We declare a configuration key ["etc-dir"] that holds the location
of the install [etc] directory.}
{- We have a pre-build hook that writes the file
[src/mypkg_etc.ml] with its actual value on [`Pin] and [`Distrib] builds.}
{- We install the [etc/mypkg.conf] configuration in the install [etc]
   directory.}}
The opam build instructions for the package are:
{[
build: [[
  "ocaml" "pkg/pkg.ml" "build"
          "--dev-pkg" "%{dev}%" # use "%{pinned}%" for opam < 2.0
          "--etc-dir" mypkg:etc ]]
]}

{2:multiopam Multiple opam packages for a single distribution}

It is not too hard to define multiple opam packages for the same
distribution. Topkg itself
{{:https://github.com/dbuenzli/topkg/blob/master/DEVEL.md}uses this
trick} to manage its dependencies between [topkg] and [topkg-care].

To achieve this your package description file can simply condition
the package install description on the package name
{{!Conf.pkg_name}communicated by the configuration}. In this setup
you'll likely have one [$PKG.opam] per [$PKG] at the root of your source
repository, you should declare them in the description too, so that
they get properly linted and used by the [topkg] tool when appropriate
(see how the opam file is looked up according to the package name
in {!Pkg.describe}). Here is a blueprint:
{[
let () =
  let opams =
    let install = false in
    [ Pkg.opam_file ~install "mypkg-main.opam";
      Pkg.opam_file ~install "mypkg-snd.opam"; ]
  in
  Pkg.describe ~opams "mypkg-main" @@ fun c ->
  match Conf.pkg_name c with
  | "mypkg-main" ->
      Ok [ Pkg.lib "mypkg-main.opam" ~dst:"opam";
           (* mypkg-main install *) ]
  | "mypkg-snd" ->
      Ok [ Pkg.lib "mypkg-snd.opam" ~dst:"opam";
           (* mypkg-snd install *) ]
  | other ->
      R.error_msgf "unknown package name: %s" other
]}
The build instructions of these opam files need to give the name of
the package to the build invocation so that the right install description
can be selected:
{[
build: [[
  "ocaml" "pkg/pkg.ml" "build"
          "--pkg-name" name
          "--dev-pkg" "%{dev}%" # use "%{pinned}%" for opam < 2.0
]]
]}

In general you will use the default, main, package name and its opam file to
create and publish the distribution archive file and all packages
will use the same distribution; the opam packages will only differ in their
opam file. Releasing the set of packages then becomes:
{[
# Release the distribution and base package (use topkg bistro
# for doing this via a single invocation)
topkg distrib
topkg publish
topkg opam pkg
topkg opam submit

# Create and release the other opam package based on the ditrib
topkg opam pkg --pkg-name mypkg-snd
topkg opam submit --pkg-name mypkg-snd
]}

See [topkg help release] for more information about releasing
packages with [topkg].

{1:menagerie Menagerie of [pkg.ml] files}

This is a menagerie of [pkg.ml] with a description of what they
showcase. The examples are approximatively sorted by increasing
complexity.

In all these packages the readme, change log and license file are
automatically installed in the directory of the [doc] field and the
ocamlfind META file and opam file of the package are automatically installed
in the directory of the [lib] field.

{{:https://github.com/dbuenzli/hmap/blob/master/pkg/pkg.ml}Hmap}
({{:https://github.com/dbuenzli/hmap/blob/master/pkg/META}META},
{{:https://github.com/dbuenzli/hmap/blob/master/opam}[opam]})
{ul {- Single module library archive [hmap]. The simplest you can get.}}

{{:https://github.com/dbuenzli/fpath/blob/master/pkg/pkg.ml}Fpath}
({{:https://github.com/dbuenzli/fpath/blob/master/pkg/META}META},
{{:https://github.com/dbuenzli/fpath/blob/master/opam}[opam]})
{ul
{- Single module library archive [fpath].}
{- Private library archive [fpath_top] for toplevel support.}}

{{:https://github.com/dbuenzli/astring/blob/master/pkg/pkg.ml}Astring}
({{:https://github.com/dbuenzli/astring/blob/master/pkg/META}META},
{{:https://github.com/dbuenzli/astring/blob/master/opam}[opam]})
{ul
{- Library archive [astring] namespaced by one module.}
{- Private library archive for toplevel support ([astring_top]).}
{- Installation of sample code in the [doc/] directory.}}

{{:https://github.com/dbuenzli/fmt/blob/master/pkg/pkg.ml}Fmt}
({{:https://github.com/dbuenzli/fmt/blob/master/pkg/META}META},
{{:https://github.com/dbuenzli/fmt/blob/master/opam}[opam]})
{ul
{- Single module library archive ([fmt]).}
{- Private library archive [fmt_top] for toplevel support.}
{- Single module library archive [fmt_tty]
   conditional on the presence of the opam [base-unix] package.}
{- Single module library archive [fmt_cli] conditional
   on the presence of the opam [cmdliner] package.}}

{{:https://github.com/dbuenzli/ptime/blob/master/pkg/pkg.ml}Ptime}
({{:https://github.com/dbuenzli/ptime/blob/master/pkg/META}META},
 {{:https://github.com/dbuenzli/ptime/blob/master/opam}opam})
{ul
{- Single module library archive [ptime].}
{- Private library archive [ptime_top] for toplevel support.}
{- Library archive [ptime_clock] targeting regular OSes using
   C stubs and installed in the [os/] subdirectory of [lib] field along
   with a private library archive [ptime_clock_top] for toplevel support.}
{- Library archive [ptime_clock] targeting
   JavaScript installed in the [jsoo/] subdirectory of [lib] field conditional
   on the presence of the opam [js_of_ocaml] package.}
{- Installation of sample code in the [doc/] directory.}}

{{:https://github.com/dbuenzli/uucp/blob/master/pkg/pkg.ml}Uucp}
({{:https://github.com/dbuenzli/uucp/blob/master/pkg/META}META},
 {{:https://github.com/dbuenzli/uucp/blob/master/opam}opam})
{ul
{- Library archive [uucp] namespaced by a single module.}
{- Custom watermark for substituting the supported Unicode version.}
{- Generation of distribution time build artefacts via a
   {{!Pkg.distrib}massage} hook which invokes an
   {{:https://github.com/dbuenzli/uucp/blob/master/pkg/build_support.ml}
   OCaml script} that downloads the UCD XML file
   and extracts compact and efficient representation of it as OCaml
   source data structures it writes in the [src/] directory.}
{- Adds the [support/] path to the {{!Pkg.distrib}paths to exclude}
   from the distribution.}
{- Installation of development information and sample code in the
   [doc/] directory.}}

{{:https://github.com/dbuenzli/carcass/blob/master/pkg/pkg.ml}Carcass}
({{:https://github.com/dbuenzli/carcass/blob/master/pkg/META}META},
 {{:https://github.com/dbuenzli/carcass/blob/master/opam}opam})
{ul
{- Library archive [carcass] namespaced by a single module.}
{- Single module library archive [carcass_cli].}
{- Executable [carcass].}
{- {{!config_store}Stores} install [etc] location in the software artefacts
   with a {{!Pkg.build}pre-build hook}.}
{- Adjusts the {{!Pkg.distrib}files to watermark} to ignore the files in the
   [etc] file hierarchy of the distribution.}
{- Installs the [etc] hierarchy of the distribution in the [etc] field
   directly from the source tree (i.e. the files are not built).}}

{{:https://github.com/dbuenzli/topkg/blob/master/pkg/pkg.ml}Topkg}
({{:https://github.com/dbuenzli/topkg/blob/master/pkg/META}META},
{{:https://github.com/dbuenzli/topkg/blob/master/topkg.opam}topkg.opam},
{{:https://github.com/dbuenzli/topkg/blob/master/topkg-care.opam}
topkg-care.opam})
{ul
{- Ignore the funky source bootstraping ([#mod_use] directives), that's
   only for using [topkg] on itself.}
{- Makes {{!multiopam}multiple opam packages} for the same
   distribution.}
{- Multiple opam file declaration and dependency linting exclusions:
   the build system mentions packages that are not relevant to
   all opam files. Manual, per package, opam file install in the [lib]
   field.}
{- Manual [META] install, a single one is installed for all opam packages
   by the base package [topkg]. This leverages the [if_exists] ocamlfind
   mecanism.}
{- The [topkg] package installs the library archive [topkg] namespaced
   by a single module.}
{- The [topkg-care] package installs the library archive [topkg-care]
   namespaced by a single module and the binaries [topkg] and
   [toy-github-topkg-delegate]}}
*)

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
