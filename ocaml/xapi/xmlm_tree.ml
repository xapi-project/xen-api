(** Conveniently output an XML tree using Xmlm *)

(** Type used for constructing XML documents *)
type tree =
  | El of Xmlm.tag * tree list
  | Data of string


(** {2 Constructor functions} *)

let data s = Data s

(** Create an element without a namespace or attributes *)
let el name children = El ((("", name), []), children)


(** {2 Outputting a {!tree} using [Xmlm]} *)

(** Creates a function deconstructing the above tree structure that can be
    passed to Xmlm's tree output functions. *)
let frag = function
  | El (tag, children) -> `El (tag, children)
  | Data s -> `Data s

(** Outputs the given tree using [Xmlm.output_doc_tree] *)
let output_doc ~tree ~output ~dtd =
  Xmlm.output_doc_tree frag output (dtd, tree)
