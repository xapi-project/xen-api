val html_of_t : 'a Rpc.Types.typ -> string list
val of_args : (bool * Idl.Param.boxed) list -> Cow.Html.t
val sidebar : Codegen.Interfaces.t -> Cow.Html.t
val of_struct_fields : 'a Rpc.Types.boxed_field list -> Cow.Html.t
val of_variant_tags : 'a Rpc.Types.boxed_tag list -> Cow.Html.t
val of_type_decl : 'a -> Rpc.Types.boxed_def -> Cow.Html.t list
val tabs_of :
  'a -> Codegen.Interface.t -> 'b Codegen.Method.t -> Cow.Html.t list
val of_method :
  'a -> Codegen.Interface.t -> Codegen.boxed_fn -> Cow.Html.t list
val of_interface : 'a -> Codegen.Interface.t -> Cow.Html.t list
val of_interfaces : Codegen.Interfaces.t -> Cow.Html.t
val to_string : Codegen.Interfaces.t -> string
