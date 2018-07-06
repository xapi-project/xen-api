(** The Stable versions of Hashtbl, Hash_set, Map, and Set are defined here rather than in
    their respective modules because:

    1. We guarantee their serializations independent of the implementation of those modules
    2. Given 1. it is cleaner (and still okay) to separate the code into a separate file *)

open! Import


module Hashable : sig
  module V1 : sig
    module type S = sig
      type key

      module Table : sig
        type 'a t = (key, 'a) Hashtbl.t [@@deriving sexp, bin_io]
      end

      module Hash_set : sig
        type t = key Hash_set.t [@@deriving sexp, bin_io]
      end
    end

    module Make (Key : Hashtbl.Key_binable) : S with type key := Key.t
  end
end
