include Deferred1

module Array    = Deferred_array
module List     = Deferred_list
module Map      = Deferred_map
module Memo     = Deferred_memo
module Option   = Deferred_option
module Or_error = Deferred_or_error
module Queue    = Deferred_queue
module Result   = Deferred_result
module Sequence = Deferred_sequence

let%test_module "Deferred_array_tests" =
  (module struct
    include Monad_sequence_unit_tests.Make
        (Core_kernel.Array)
        (Array)
  end)

let%test_module "Deferred_sequence_tests" =
  (module struct
    include Monad_sequence_unit_tests.Make
        (Core_kernel.Sequence)
        (Sequence)
  end)

let%test_module "Deferred_queue_tests" =
  (module struct
    include Monad_sequence_unit_tests.Make
        (struct
          include Core_kernel.Queue
          let compare cmp t1 t2 = Core_kernel.List.compare cmp (to_list t1) (to_list t2)
          let concat_map t ~f = concat_map t ~f:(fun x -> to_list (f x))
          let concat_mapi t ~f = concat_mapi t ~f:(fun i x -> to_list (f i x))
        end)
        (Queue)
  end)


let%test_module "Deferred_list_tests" =
  (module struct
    include Monad_sequence_unit_tests.Make
        (Core_kernel.List)
        (List)
  end)
