val product_version : unit -> string

val product_version_text : unit -> string

val product_version_text_short : unit -> string

val platform_name : unit -> string

val platform_version : unit -> string

val product_brand : unit -> string

val build_number : unit -> string

val hostname : string

val date : string

val version : string

val git_id : string

val xapi_version_major : int

val xapi_version_minor : int

val compare_to_local : string -> int
(** [compare_to_local v] compares [xapi_version_major] and
    [xapi_version_minor], the major and minor numbers of [version], with
    those of [v]. Raises [Failure] if [v] cannot be parsed. *)

val xapi_user_agent : string

val arg_spec : string * Arg.spec * string
(** A --version option for the Arg module, which prints [version] and exits *)
