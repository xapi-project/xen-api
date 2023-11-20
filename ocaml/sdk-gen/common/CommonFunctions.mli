(** Exception for unknown wire protocol. *)
exception Unknown_wire_protocol

(** Type representing supported protocols. *)
type wireProtocol = XmlRpc | JsonRpc

val get_release_branding : string -> string
(** [get_release_branding codename] Gets the branding for a release codename.
    @param codename Release codename to lookup.
    @return Branding for the release codename, or the original codename if not found. *)

val is_setter : Datamodel_types.message -> bool
(** [is_setter message] Checks if a message is a setter based on its name.
    @param message Message to check.
    @return [true] if the message is a setter, [false] otherwise. *)

val is_getter : Datamodel_types.message -> bool
(** [is_getter message] Checks if a message is a getter based on its name.
    @param message Message to check.
    @return [true] if the message is a getter, [false] otherwise. *)

val get_deprecated_info_message : Datamodel_types.message -> string
(** [get_deprecated_info_message message] Returns a deprecated information message
    based on the internal_deprecated_since version in the input message.
    @param message Message containing version information.
    @return Deprecated information message or an empty string if not deprecated. *)

val is_adder : Datamodel_types.message -> bool
(** [is_adder message] Checks if a message is an adder based on its name.
    @param message Message to check.
    @return [true] if the message is an adder, [false] otherwise. *)

val get_published_info_class : Datamodel_types.obj -> string
(** [get_published_info_class cls] Returns information about the first publication
    of a class based on its lifecycle transitions.
    @param cls Class to retrieve publication information for.
    @return Information string with the first published version. *)

val is_method_static : Datamodel_types.message -> bool
(** [is_method_static message] Checks if a method is static based on its parameters.
    @param message Message to check.
    @return [true] if the method is static, [false] otherwise. *)

val escape_xml : string -> string
(** [escape_xml s] Escapes XML special characters in a string.
    @param s String to escape.
    @return String with XML special characters escaped. *)

val get_published_info_message :
  Datamodel_types.message -> Datamodel_types.obj -> string
(** Gets information about the publication status of a message.
    @param message - Message to check.
    @param cls - Class containing the message.
    @return Information about the publication status. *)

val is_remover : Datamodel_types.message -> bool
(** [is_remover message] Checks if a message is a remover based on its name.
    @param message Message to check.
    @return [true] if the message is a remover, [false] otherwise. *)

val gen_param_groups :
     Datamodel_types.message
  -> Datamodel_types.param list
  -> Datamodel_types.param list list
(** Generates parameter groups based on a message and its parameters.
    @param message - Message containing the parameters.
    @param params - List of parameters.
    @return List of parameter groups. *)

val get_published_info_param :
  Datamodel_types.message -> Datamodel_types.param -> string
(** Gets information about the publication status of a parameter.
    @param message - Message containing the parameter.
    @param param - Parameter to check.
    @return Information about the publication status. *)

val get_published_info_field :
  Datamodel_types.field -> Datamodel_types.obj -> string
(** Gets information about the publication status of a field within a class.
    @param field - Field to check.
    @param cls - Class containing the field.
    @return Information about the publication status. *)

val string_of_file : string -> string
(** [string_of_file filename] Reads the content of a file into a string.
    @param filename Name of the file to read.
    @return String containing the file content. *)

val is_constructor : Datamodel_types.message -> bool
(** [is_constructor message] Checks if a message is a constructor.
    @param message Message to check.
    @return [true] if the message is a constructor, [false] otherwise. *)

val with_output : string -> (out_channel -> 'a) -> 'a
(** [with_output filename f] Opens a file for writing and executes a function with the output channel.
    @param filename Name of the file to open.
    @param f Function to execute with the output channel. *)

val is_destructor : Datamodel_types.message -> bool
(** [is_destructor message] Checks if a message is a destructor.
    @param message Message to check.
    @return [true] if the message is a destructor, [false] otherwise. *)

val is_real_constructor : Datamodel_types.message -> bool
(** [is_real_constructor message] Checks if a message is a real constructor.
    @param message Message to check.
    @return [true] if the message is a real constructor, [false] otherwise. *)

val render_file : string * string -> Mustache.Json.t -> string -> string -> unit
(** [render_file (infile, outfile) json templates_dir dest_dir] Renders a file using a JSON data model and templates.
    @param infile Input file name.
    @param outfile Output file name.
    @param json JSON data model.
    @param templates_dir Directory containing templates.
    @param dest_dir Directory for the rendered output. *)

val json_releases : Mustache.Json.t
(** JSON structure representing release information. *)
