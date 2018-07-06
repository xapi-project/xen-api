open! Import
open Hashtbl_intf

module Avltree = Avltree
module Binable = Binable0
module Hashable = Hashtbl_intf.Hashable
module List = List0

let failwiths = Error.failwiths

module Creators = Hashtbl.Creators
include (Hashtbl : sig
           type ('a, 'b) t = ('a, 'b) Hashtbl.t [@@deriving sexp_of]
           include Base.Hashtbl.S_without_submodules
             with type ('a, 'b) t := ('a, 'b) t
             with type 'b merge_into_action = 'b Hashtbl.merge_into_action
         end)

module Using_hashable = struct
  type nonrec ('a, 'b) t = ('a, 'b) t [@@deriving sexp_of]
  let create ?growth_allowed ?size ~hashable () =
    create ?growth_allowed ?size (Base.Hashable.to_key hashable)
  let of_alist ?growth_allowed ?size ~hashable l =
    of_alist ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  let of_alist_report_all_dups ?growth_allowed ?size ~hashable l =
    of_alist_report_all_dups ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  let of_alist_or_error ?growth_allowed ?size ~hashable l =
    of_alist_or_error ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  let of_alist_exn ?growth_allowed ?size ~hashable l =
    of_alist_exn ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  let of_alist_multi ?growth_allowed ?size ~hashable l =
    of_alist_multi ?growth_allowed ?size (Base.Hashable.to_key hashable) l
  let create_mapped ?growth_allowed ?size ~hashable ~get_key ~get_data l =
    create_mapped ?growth_allowed ?size (Base.Hashable.to_key hashable) ~get_key ~get_data l
  let create_with_key ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key ?growth_allowed ?size (Base.Hashable.to_key hashable) ~get_key l
  let create_with_key_or_error ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key_or_error ?growth_allowed ?size (Base.Hashable.to_key hashable) ~get_key l
  let create_with_key_exn ?growth_allowed ?size ~hashable ~get_key l =
    create_with_key_exn ?growth_allowed ?size (Base.Hashable.to_key hashable) ~get_key l
  let group ?growth_allowed ?size ~hashable ~get_key ~get_data ~combine l =
    group ?growth_allowed ?size (Base.Hashable.to_key hashable) ~get_key ~get_data ~combine l
end

module type S_plain   = S_plain  with type ('a, 'b) hashtbl = ('a, 'b) t
module type S         = S         with type ('a, 'b) hashtbl = ('a, 'b) t
module type S_binable = S_binable with type ('a, 'b) hashtbl = ('a, 'b) t

module type Key_plain = Key_plain
module type Key = Key
module type Key_binable = Key_binable

module Poly = struct
  include Hashtbl.Poly

  include Bin_prot.Utils.Make_iterable_binable2 (struct
      type ('a, 'b) z = ('a, 'b) t
      type ('a, 'b) t = ('a, 'b) z
      type ('a, 'b) el = 'a * 'b [@@deriving bin_io]

      let caller_identity = Bin_prot.Shape.Uuid.of_string "8f3e445c-4992-11e6-a279-3703be311e7b"
      let module_name = Some "Core_kernel.Hashtbl"
      let length = length
      let iter t ~f = iteri t ~f:(fun ~key ~data -> f (key, data))
      let init ~len ~next =
        let t = create ~size:len () in
        for _i = 0 to len - 1 do
          let key,data = next () in
          match find t key with
          | None -> set t ~key ~data
          | Some _ -> failwith "Core_hashtbl.bin_read_t_: duplicate key"
        done;
        t
      ;;
    end)
end

module Make_plain (Key : Key_plain) = struct

  let hashable =
    { Hashable.
      hash = Key.hash;
      compare = Key.compare;
      sexp_of_t = Key.sexp_of_t;
    }
  ;;

  type key = Key.t
  type ('a, 'b) hashtbl = ('a, 'b) t
  type 'a t = (Key.t, 'a) hashtbl
  type ('a, 'b) t__ = (Key.t, 'b) hashtbl
  type 'a key_ = Key.t

  include Creators (struct
      type 'a t = Key.t
      let hashable = hashable
    end)

  include (Hashtbl : sig
             include Hashtbl.Accessors
               with type ('a, 'b) t := ('a, 'b) t__
               with type 'a key := 'a key_
             include Hashtbl.Multi
               with type ('a, 'b) t := ('a, 'b) t__
               with type 'a key := 'a key_
             include Invariant.S2 with type ('a, 'b) t := ('a, 'b) hashtbl
           end
          )

  let invariant invariant_key t = invariant ignore invariant_key t

  let sexp_of_t sexp_of_v t = Poly.sexp_of_t Key.sexp_of_t sexp_of_v t

  module Provide_of_sexp (Key : sig type t [@@deriving of_sexp] end with type t := key) =
  struct
    let t_of_sexp v_of_sexp sexp = t_of_sexp Key.t_of_sexp v_of_sexp sexp
  end

  module Provide_bin_io (Key' : sig type t [@@deriving bin_io] end with type t := key) =
    Bin_prot.Utils.Make_iterable_binable1 (struct
      module Key = struct include Key include Key' end
      type nonrec 'a t = 'a t
      type 'a el = Key.t * 'a [@@deriving bin_io]

      let caller_identity = Bin_prot.Shape.Uuid.of_string "8fabab0a-4992-11e6-8cca-9ba2c4686d9e"
      let module_name = Some "Core_kernel.Hashtbl"
      let length = length
      let iter t ~f = iteri t ~f:(fun ~key ~data -> f (key, data))

      let init ~len ~next =
        let t = create ~size:len () in
        for _i = 0 to len - 1 do
          let (key, data) = next () in
          match find t key with
          | None -> set t ~key ~data
          | Some _ -> failwiths "Hashtbl.bin_read_t: duplicate key" key [%sexp_of: Key.t]
        done;
        t
      ;;
    end)
end

module Make (Key : Key) = struct
  include Make_plain (Key)
  include Provide_of_sexp (Key)
end

module Make_binable (Key : Key_binable) = struct
  include Make (Key)
  include Provide_bin_io (Key)
end

module M = Hashtbl.M

let hashable = Hashtbl.Private.hashable

let iter_vals = iter
let replace = set
let replace_all = map_inplace
let replace_alli = mapi_inplace
let filter_replace_all = filter_map_inplace
let filter_replace_alli = filter_mapi_inplace

let%test_unit _ = (* [sexp_of_t] output is sorted by key *)
  let module Table =
    Make (struct
      type t = int [@@deriving bin_io, compare, sexp]
      let hash (x : t) = if x >= 0 then x else ~-x
    end)
  in
  let t = Table.create () in
  for key = -10 to 10; do
    Table.add_exn t ~key ~data:();
  done;
  List.iter
    [ [%sexp_of: unit Table.t]
    ; [%sexp_of: (int, unit) t]
    ]
    ~f:(fun sexp_of_t ->
      let list =
        t
        |> [%sexp_of: t]
        |> [%of_sexp: (int * unit) list]
      in
      assert (List.is_sorted list ~compare:(fun (i1, _) (i2, _) -> i1 - i2)))
;;
