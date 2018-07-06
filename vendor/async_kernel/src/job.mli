open! Core_kernel
open! Import

type t = Types.Job.t [@@deriving sexp_of]

include Invariant.S with type t := t
