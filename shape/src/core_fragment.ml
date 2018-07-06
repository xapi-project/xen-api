(* fragment of [Core] interface needed by [bin_shape] runtime *)

include Ppx_compare_lib.Builtin
include Ppx_sexp_conv_lib.Conv

open Printf
(* [sort_uniq] is missing from the [List] exposed by [StdLabels] *)
let sort_uniq = List.sort_uniq
open StdLabels

module Caml = struct
  module Digest = Digest
end

module Sexp = Base.Sexp

let failwithf fmt = ksprintf (fun s () -> failwith s) fmt

module Identifiable = struct
  module type S = sig
    type t [@@deriving sexp]
    val compare : t -> t -> int
    val (=) : t -> t -> bool
    val of_string : string -> t
    val to_string : t -> string
  end
end

module String = struct
  include (String : module type of struct include String end with type t := string)
  type t = string [@@deriving sexp]
  let (=) : (t -> t -> bool) = (=)
  let of_string x = x
  let to_string x = x
end

module Int = struct
  type t = int
  let (=) : (t -> t -> bool) = (=)
end

module Option = struct
  let iter ~f = function | None -> () | Some x -> f x
  let map ~f = function | None -> None | Some x -> Some (f x)
  let exists ~f = function | None -> false | Some x -> f x
end

module List = struct
  include List
  let sort_uniq ~compare l = sort_uniq ~cmp:compare l
  module Assoc = struct
    let find xs ~equal key =
      try Some (snd (List.find xs ~f:(fun (k,_) -> equal k key)))
      with Not_found -> None
  end
  let concat_map xs ~f = concat (List.map xs ~f)
  let zip l1 l2 = try Some (map2 ~f:(fun a b -> (a, b)) l1 l2) with _ -> None
  let fold = fold_left
end
