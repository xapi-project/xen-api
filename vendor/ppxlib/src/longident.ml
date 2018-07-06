open! Import

module T = struct
  type t = longident =
      Lident of string
    | Ldot of t * string
    | Lapply of t * t

  let compare : t -> t -> int = Polymorphic_compare.compare

  let rec name = function
    | Lident s -> s
    | Ldot (a, b) -> name a ^ "." ^ b
    | Lapply (a, b) -> Printf.sprintf "%s(%s)" (name a) (name b)

  let sexp_of_t t = Sexp.Atom (name t)
end
include T

include Comparable.Make(T)

let rec flat accu = function
    Lident s -> s :: accu
  | Ldot(lid, s) -> flat (s :: accu) lid
  | Lapply(_, _) -> invalid_arg "Ppxlib.Longident.flatten"

let flatten_exn lid = flat [] lid

let last_exn = function
    Lident s -> s
  | Ldot(_, s) -> s
  | Lapply(_, _) -> invalid_arg "Ppxlib.Longident.flatten"

let parse s =
  match String.split s ~on:'.' with
  | [] -> assert false
  | s :: l -> List.fold_left l ~init:(Lident s) ~f:(fun acc s -> Ldot (acc, s))
