open Core
open Core_bench.Std

module Always = Type_immediacy.Always

module M = struct
  type t = A | B | C [@@deriving typerep]

  let always = Option.value_exn (Always.of_typerep typerep_of_t)
end

let tests =
  [
    Bench.Test.create ~name:"Always.value_as_int"
      (fun () -> ignore (Always.value_as_int M.always M.A))
  ]
;;

let () = Command.run (Bench.make_command tests)
