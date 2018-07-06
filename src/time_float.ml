open! Import
open! Std_internal

module T = Time.Make (Time_float0)

module Stable = struct
  include Time_float0.Stable

  module With_utc_sexp = struct
    (* V2 is actually the first version of this in Core_kernel, but a V1 stable module
       with generous parsing, unix-dependent [t_of_sexp] already existed in Core *)
    module V2 = struct
      type t = T.t [@@deriving bin_io, compare]

      let sexp_of_t t =
        [%sexp (T.to_string_abs_parts t ~zone:Zone.utc : string list)]

      let t_of_sexp sexp =
        try
          match sexp with
          | Sexp.List [Sexp.Atom date; Sexp.Atom ofday_and_possibly_zone] ->
            T.of_string (date ^ " " ^ ofday_and_possibly_zone)
          | _ -> of_sexp_error "Time.Stable.With_utc.V2.t_of_sexp" sexp
        with
        | Of_sexp_error _ as e -> raise e
        | e ->
          of_sexp_error
            (sprintf "Time.Stable.With_utc.V2.t_of_sexp: %s" (Exn.to_string e))
            sexp
    end
  end

  module Zone = Zone.Stable
end

include T
