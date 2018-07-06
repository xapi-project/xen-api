open! Import
open Std_internal
open Validated_intf

module type Raw = Raw

type ('raw, 'witness) t = 'raw

module type S = S
  with type ('a, 'b) validated := ('a, 'b) t
module type S_bin_io_compare_hash_sexp = S_bin_io_compare_hash_sexp
  with type ('a, 'b) validated := ('a, 'b) t

let raw t = t

module Make (Raw : Raw) = struct

  type witness

  type t = Raw.t [@@deriving sexp_of]

  let validation_failed t error =
    Error.create "validation failed" (t, error, Raw.here)
      [%sexp_of: Raw.t * Error.t * Source_code_position.t]
  ;;

  let create_exn t =
    match Validate.result (Raw.validate t) with
    | Ok () -> t
    | Error error -> Error.raise (validation_failed t error)
  ;;

  let create t =
    match Validate.result (Raw.validate t) with
    | Ok () -> Ok t
    | Error error -> Error (validation_failed t error)
  ;;

  let t_of_sexp sexp = create_exn (Raw.t_of_sexp sexp)

  let raw t = t

end

module Add_bin_io
    (Raw : sig
       type t [@@deriving bin_io]
       include Raw_bin_io with type t := t
     end)
    (Validated : S with type raw := Raw.t) = struct
  include Binable.Of_binable (Raw) (struct
      type t = Raw.t
      let of_binable raw =
        if Raw.validate_binio_deserialization
        then Validated.create_exn raw
        else raw
      ;;
      let to_binable = Fn.id
    end)
end

module Add_compare
    (Raw : sig
       type t [@@deriving compare]
       include Raw with type t := t
     end)
    (Validated : S with type raw := Raw.t) = struct
  let compare t1 t2 = [%compare: Raw.t] (raw t1) (raw t2)
end

module Add_hash
    (Raw : sig
       type t [@@deriving hash]
       include Raw with type t := t
     end)
    (Validated : S with type raw := Raw.t) = struct
  let hash_fold_t state t = Raw.hash_fold_t state (Validated.raw t)
  let hash t = Raw.hash (Validated.raw t)
end

module Add_typerep
    (Raw : sig
       type t [@@deriving typerep]
       include Raw with type t := t
     end)
    (Validated : S with type raw := Raw.t) = struct
  type t = Raw.t [@@deriving typerep]
end

module Make_binable (Raw : Raw_bin_io) = struct
  module T0 = Make (Raw)

  include T0
  include Add_bin_io (Raw) (T0)
end

module Make_bin_io_compare_hash_sexp
    (Raw : sig
       type t [@@deriving compare, hash]
       include Raw_bin_io with type t := t
     end) = struct
  module T = Make_binable (Raw)

  include T
  include Add_compare (Raw) (T)
  include (Add_hash (Raw) (T) : sig type t [@@deriving hash] end with type t := t)
end

let%test_module _ =
  (module struct

    module Positive_int = struct
      type t = int [@@deriving bin_io, sexp]
      let validate t =
        if t > 0
        then Validate.pass
        else Validate.fail "must be positive"
      ;;
    end

    let does_raise = Exn.does_raise

    (* The [: S] is to remind us to add a unit test whenever the [S]
       interface changes. *)
    module M : S with type raw := int = struct

      module M = Make (struct
          let here = [%here]
          include Positive_int
        end)

      open M

      type witness

      type nonrec t = t

      let t_of_sexp = t_of_sexp
      let sexp_of_t = sexp_of_t

      let%test_unit _ = assert (does_raise (fun () -> t_of_sexp ([%sexp_of: int] 0)))

      let%test_unit _ =
        let sexp = [%sexp_of: int] 13 in
        assert (sexp_of_t (t_of_sexp sexp) = sexp)
      ;;

      let create     = create
      let create_exn = create_exn
      let raw        = raw

      let%test_unit _ = assert (does_raise (fun () -> create_exn 0))

      let%test_unit _ =
        match create 0 with
        | Error _ -> ()
        | Ok _ -> assert false
      ;;

      let%test_unit _ =
        let n = 13 in
        let t = create_exn n in
        assert (raw t = n)
      ;;

      let%test_unit _ =
        let n = 13 in
        match create n with
        | Error _ -> assert false
        | Ok t -> assert ((t :> int) = n)
      ;;
    end

    module M1 = Make_binable (struct
        let here = [%here]
        let validate_binio_deserialization = true
        include Positive_int
      end)

    module M2 = Make_binable (struct
        let here = [%here]
        let validate_binio_deserialization = false
        include Positive_int
      end)

    let int = 0
    let string = Binable.to_string (module Int) int
    let%test _ = does_raise (fun () -> Binable.of_string (module M1) string)
    let%test _ = (Binable.of_string (module M2) string) = int

    let int = 1
    let string = Binable.to_string (module Int) int
    let%test _ = Binable.of_string (module M1) string = int
    let%test _ = Binable.of_string (module M2) string = int

  end)
