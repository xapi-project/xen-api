type indexrec = {name_label:string option; uuid: string; _ref: string}
val string_of : indexrec -> string
val insert : indexrec -> unit
val remove : string (* ref or uuid *) -> unit
val update_name_label : string (* ref *) -> string -> unit
val update_uuid : string (* ref *) -> string -> unit
val lookup : string (* ref or uuid *) -> indexrec option
