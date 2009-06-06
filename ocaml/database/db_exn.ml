
exception Duplicate_key of (*class*) string * (*field*) string * (*uuid*) string * (*key*) string
exception DBCache_NotFound of string*string*string
exception Uniqueness_constraint_violation of string*string*string
