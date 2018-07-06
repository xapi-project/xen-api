module T = struct
  include Bin_prot.Md5
  let sexp_of_t t = t |> to_hex |> String.sexp_of_t
  let t_of_sexp s = s |> String.t_of_sexp |> of_hex_exn
end

let hash_fold_t accum t = String.hash_fold_t accum (T.to_binary t)
let hash t = String.hash (T.to_binary t)

module As_binary_string = struct
  module Stable = struct
    module V1 = struct
      type t = T.t [@@deriving compare]

      let hash_fold_t = hash_fold_t
      let hash = hash

      let sexp_of_t x = String.sexp_of_t (T.to_binary x)
      let t_of_sexp x = T.of_binary_exn (String.t_of_sexp x)

      include Bin_prot.Utils.Make_binable (struct
          module Binable = String
          type t = Bin_prot.Md5.t
          let to_binable = T.to_binary
          let of_binable = T.of_binary_exn
        end)

    end
  end
  include Stable.V1

  include Comparable.Make(Stable.V1)
  include Hashable.Make(Stable.V1)
end

module Stable = struct

  module V1 = struct
    type t = T.t [@@deriving compare, sexp]

    let hash_fold_t = hash_fold_t
    let hash = hash

    include Bin_prot.Utils.Make_binable (struct
        module Binable = Bin_prot.Md5
        type t = Bin_prot.Md5.t
        let to_binable = Fn.id
        let of_binable = Fn.id
      end)

  end

  let digest_string s = Md5_lib.string s

  let%test_module "Md5.As_binary_string.V1" = (module Stable_unit_test.Make(struct
      include As_binary_string.Stable.V1
      let equal = [%compare.equal: t]
      let tests =
        [ digest_string ""
        , {|"\212\029\140\217\143\000\178\004\233\128\t\152\236\248B~"|}
        , "\016\212\029\140\217\143\000\178\004\233\128\t\152\236\248B~";
          digest_string "x"
        , {|"\157\212\228a&\140\1284\245\200VN\021\\g\166"|}
        , "\016\157\212\228a&\140\1284\245\200VN\021\\g\166"
        ]
    end))

  let%test_module "Md5.V1" = (module Stable_unit_test.Make(struct
      include V1
      let equal = [%compare.equal: t]
      let tests =
        [ digest_string ""
        , "d41d8cd98f00b204e9800998ecf8427e"
        , "\212\029\140\217\143\000\178\004\233\128\t\152\236\248B~";
          digest_string "x"
        , "9dd4e461268c8034f5c8564e155c67a6"
        , "\157\212\228a&\140\1284\245\200VN\021\\g\166"
        ]
    end))
end

include Stable.V1

include Comparable.Make(Stable.V1)
include Hashable.Make(Stable.V1)

let digest_num_bytes = 16

let to_hex = T.to_hex
let from_hex = T.of_hex_exn
let of_hex_exn = T.of_hex_exn

let of_binary_exn = T.of_binary_exn
let to_binary = T.to_binary

let digest_string = Stable.digest_string
let digest_bytes = Md5_lib.bytes
let digest_file_blocking_without_releasing_runtime_lock f = of_binary_exn (Caml.Digest.file f)
let file = digest_file_blocking_without_releasing_runtime_lock
let digest_channel_blocking_without_releasing_runtime_lock channel ~len =
  of_binary_exn (Caml.Digest.channel channel len)
let channel channel len =
  digest_channel_blocking_without_releasing_runtime_lock channel ~len
let output_blocking t oc = Caml.Digest.output oc (to_binary t)
let output oc t = output_blocking t oc
let input_blocking ic = of_binary_exn (Caml.Digest.input ic)
let input = input_blocking
let digest_subbytes = Md5_lib.subbytes
let string = digest_string
let bytes = digest_bytes
let subbytes s pos len = digest_subbytes s ~pos ~len

let digest_bin_prot writer value =
  digest_string (Core_bin_prot.Writer.to_string writer value)
