let reference = "_ref"

(** Take a field name as a list (including namespaces) and return a flat name *)
let escape_id x = String.concat "__" x

(** Object/class names are used as-is *)
let escape_obj x = x
