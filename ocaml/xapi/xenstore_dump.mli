exception Invalid_path of string

(** Dump a xenstore subtree as XML *)
val dump : xs:Xs.xsh -> string -> Xml.xml

(** Restore a xenstore subtree from XML at a new path. Permissions are not restored
    and therefore will inherit from the parent node. *)
val restore : xs:Xs.xsh -> string -> Xml.xml -> unit
