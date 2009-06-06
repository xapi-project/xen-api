
type _val = 
    | Field of string
    | Literal of string

(** Represent a predicate: table row -> bool *)
type expr = 
    | True
    | False
    | Not of expr
    | Eq of _val * _val
    | And of expr * expr    
    | Or of expr * expr

