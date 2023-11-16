val get_http_action_stem : string -> string
(** Gets the HTTP action stem based on the name.
    @param name - Name to analyze.
    @return HTTP action stem. *)

val get_http_action_verb : string -> Datamodel.http_meth -> string
(** Gets the HTTP action verb based on the name and method.
    @param name - Name to analyze.
    @param meth - HTTP method.
    @return HTTP action verb. *)

val get_common_verb_category : string -> string
(** Gets the common verb category based on the HTTP action verb.
    @param verb - HTTP action verb.
    @return Common verb category. *)

val pascal_case_ : string -> string
(** Recursively converts a string to PascalCase.
    @param s - String to convert.
    @return PascalCase formatted string. *)

val qualified_class_name : string -> string
(** Gets the qualified class name by prepending "XenAPI." to the exposed class name.
    @param classname - Class name.
    @return Qualified class name. *)

val ocaml_class_to_csharp_local_var : string -> string
(** Converts an OCaml class name to a C# local variable.
    @param classname - OCaml class name.
    @return C# local variable name. *)

val ocaml_class_to_csharp_property : string -> string
(** Converts an OCaml class name to a C# property name.
    @param classname - OCaml class name.
    @return C# property name. *)

val ocaml_class_to_csharp_class : string -> string
(** Converts an OCaml class name to a C# class name.
    @param classname - OCaml class name.
    @return C# class name. *)

val is_invoke : Datamodel_types.message -> bool
(** Checks if a message is an invoke operation.
    @param message - Message to check.
    @return true if it's an invoke operation, false otherwise. *)

val has_uuid : Datamodel_types.obj -> bool
(** Checks if an object has a UUID field, and therefore should have a get_by_uuid message.
    @param x - Object to check.
    @return true if the object has a UUID field, false otherwise. *)

val has_name : Datamodel_types.obj -> bool
(** Checks if an object has a name field.
    @param x - Object to check.
    @return true if the object has a name field, false otherwise. *)

val full_name : Datamodel_types.field -> string
(** Gets the full name of a field as a single string with underscores escaped.
    @param field - Field to extract the full name from.
    @return Full name with underscores escaped. *)

val obj_internal_type : Datamodel_types.ty -> string
(** Gets the internal type representation of an object.
    @param x - Object to determine the internal type for.
    @return Internal type representation as a string. *)

val ocaml_field_to_csharp_property : Datamodel_types.field -> string
(** Converts an OCaml field to its corresponding C# property name.
    @param field - OCaml field.
    @return C# property name. *)

val exposed_type : Datamodel_types.ty -> string
(** Converts an exposed type from OCaml to its corresponding C# type.
    @param ty - OCaml type.
    @return Corresponding C# type. *)

val pascal_case : string -> string
(** Converts a string to PascalCase.
    @param s - Input string.
    @return PascalCase formatted string. *)

val exposed_class_name : string -> string
(** Converts an OCaml class name to a corresponding exposed class name in C#.
    @param classname - OCaml class name.
    @return Exposed class name in C#. *)

val cut_msg_name : string -> string -> string
(** Extracts the base name from an OCaml message name by removing the specified prefix.
   Some adders/removers are just prefixed by Add or RemoveFrom
      and some are prefixed by AddTo or RemoveFrom
      @param message_name - OCaml message name.
      @param fn_type - Prefix to remove ("Add" or "Remove").
      @return Base name after removing the specified prefix. *)

val lower_and_underscore_first : string -> string
(** Converts a string to lowercase and adds an underscore at the beginning.
   @param s - Input string.
   @return String in lowercase with an underscore at the beginning. *)
