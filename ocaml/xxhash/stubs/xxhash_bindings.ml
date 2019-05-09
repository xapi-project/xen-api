open Ctypes

module type BINDINGS = sig
  type hash
  type internal

  val default_seed : hash
  val hash_of_internal : internal -> hash
  val internal_of_hash : hash -> internal

  val hash : string -> Unsigned.size_t -> internal -> internal

  type state

  val create : unit -> state
  val free : state -> int

  val reset : state -> internal -> int
  val update : state -> string -> Unsigned.size_t -> int
  val digest : state -> internal
end

module C (F : Cstubs.FOREIGN) = struct
  open F

  module XXH32 = struct
    type hash = nativeint
    type internal = Unsigned.uint

    let default_seed = Nativeint.zero
    let hash_of_internal i = Unsigned.UInt.to_int64 i |> Int64.to_nativeint
    let internal_of_hash h = Int64.of_nativeint h |> Unsigned.UInt.of_int64

    let hash = F.foreign "XXH32" (string @-> size_t @-> uint @-> returning uint)

    type state_s
    type state = state_s structure ptr

    let state_t : state_s structure typ = structure "XXH32_state_s"

    let create = F.foreign "XXH32_createState" (void @-> returning (ptr state_t))
    let free = F.foreign "XXH32_freeState" (ptr state_t @-> returning int)

    let reset = F.foreign "XXH32_reset" (ptr state_t @-> uint @-> returning int)
    let update = F.foreign "XXH32_update" (ptr state_t @-> string @-> size_t @-> returning int)
    let digest = F.foreign "XXH32_digest" (ptr state_t @-> returning uint)
  end

  module XXH64 = struct
    type hash = int64
    type internal = Unsigned.ullong

    let default_seed = 0L
    let hash_of_internal = Unsigned.ULLong.to_int64
    let internal_of_hash = Unsigned.ULLong.of_int64

    let hash = F.foreign "XXH64" (string @-> size_t @-> ullong @-> returning ullong)

    type state_s
    type state = state_s structure ptr

    let state_t : state_s structure typ = structure "XXH64_state_s"

    let create = F.foreign "XXH64_createState" (void @-> returning (ptr state_t))
    let free = F.foreign "XXH64_freeState" (ptr state_t @-> returning int)

    let reset = F.foreign "XXH64_reset" (ptr state_t @-> ullong @-> returning int)
    let update = F.foreign "XXH64_update" (ptr state_t @-> string @-> size_t @-> returning int)
    let digest = F.foreign "XXH64_digest" (ptr state_t @-> returning ullong)
  end
end
