(*
 * Copyright (C) 2013-2015 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Sexplib.Std
open Result
open Lwt

(* The format will be:
   sector 0: signature (to help us know if the data is supposed to be valid
   sector 1: producer pointer (as a byte offset)
   sector 2: consumer pointer (as a byte offset)
   Each data item shall be prefixed with a 4-byte length, followed by
   the payload and padded to the next sector boundary. *)

let sector_signature = 0L
let sector_producer  = 1L
let sector_consumer  = 2L
let sector_data      = 3L

let minimum_size_sectors = Int64.add sector_data 1L

let magic = Printf.sprintf "mirage shared-block-device 1.0"

let zero buf =
  for i = 0 to Cstruct.len buf - 1 do
    Cstruct.set_uint8 buf i 0
  done

let int_of_bool = function
  | true -> 1
  | false -> 2

let bool_of_int = function
  | 1 -> Ok true
  | 2 -> Ok false
  | x -> Error (`Msg (Printf.sprintf "Failed to unmarshal a bool: %d" x))

let alloc sector_size =
  let page = Io_page.(to_cstruct (get 1)) in
  let sector = Cstruct.sub page 0 sector_size in
  sector

module Common(Log: S.LOG)(B: S.BLOCK) = struct
  type error = [ `Retry | `Suspended | `Msg of string ]

  (*BISECT-IGNORE-BEGIN*)
  let pp_error fmt = function
    | `Retry -> Format.pp_print_string fmt "Retry"
    | `Suspended -> Format.pp_print_string fmt "Suspended"
    | `Msg x -> Format.pp_print_string fmt x
  let error_to_msg = function
    | Ok x -> Ok x
    | Error `Retry -> Error (`Msg "There was a transient failure and the operation should be retried")
    | Error `Suspended -> Error (`Msg "There was a transient failure caused by the ring being suspended")
    | Error (`Msg x) -> Error (`Msg x)
  let open_error = function
    | Ok x -> Ok x
    | Error `Retry -> Error `Retry
    | Error `Suspended -> Error `Suspended
    | Error (`Msg x) -> Error (`Msg x)
  type 'a result = ('a, error) Result.result
  (*BISECT-IGNORE-END*)

  module ResultM = struct
    let (>>=) m f = Lwt.bind m (function
      | Ok x -> f x
      | Error x -> return (Error x)
      )
    let return x = Lwt.return (Ok x)
  end
  open ResultM

  module Lwt_write_error = struct
    let (>>=) m f =
      let open Lwt.Infix in
      m >>= function
      | Error `Is_read_only -> Lwt.return (Error `Is_read_only)
      | Error `Disconnected -> Lwt.return (Error `Disconnected)
      | Error `Unimplemented -> Lwt.return (Error `Unimplemented)
      | Error x -> Lwt.return (Error x)
      | Ok x -> f x
    let to_msg m =
      let open Lwt.Infix in
      m >>= function
      | Error `Disconnected -> Lwt.return (Error (`Msg "BLOCK device has already disconnected"))
      | Error `Unimplemented -> Lwt.return (Error (`Msg "BLOCK function is unimplemented"))
      | Error `Is_read_only -> Lwt.return (Error (`Msg "BLOCK device is read-only"))
      | Error _ -> Lwt.return (Error (`Msg "Unknown error from BLOCK device"))
      | Ok x -> Lwt.return (Ok x)
    let return x = Lwt.return (Ok x)
  end
  module Lwt_read_error = struct
    let (>>=) m f =
      let open Lwt.Infix in
      m >>= function
      | Error `Disconnected -> Lwt.return (Error `Disconnected)
      | Error `Unimplemented -> Lwt.return (Error `Unimplemented)
      | Error x -> Lwt.return (Error x)
      | Ok x -> f x
    let to_msg m =
      let open Lwt.Infix in
      m >>= function
      | Error `Disconnected -> Lwt.return (Error (`Msg "BLOCK device has already disconnected"))
      | Error `Unimplemented -> Lwt.return (Error (`Msg "BLOCK function is unimplemented"))
      | Error _ -> Lwt.return (Error (`Msg "Unknown error from BLOCK device"))
      | Ok x -> Lwt.return (Ok x)
    let return x = Lwt.return (Ok x)
  end

  let trace list =
    Lwt.bind (Log.trace list) (fun () -> Lwt.return (Ok ()))

  let initialise device sector =
    (* Initialise the producer and consumer before writing the magic
       in case we crash in the middle *)
    zero sector;
    Cstruct.set_uint8 sector 8 (int_of_bool false);
    let open Lwt_write_error in
    B.write device sector_producer [ sector ] >>= fun () ->
    B.write device sector_consumer [ sector ] >>= fun () ->
    Cstruct.blit_from_string magic 0 sector 0 (String.length magic);
    B.write device sector_signature [ sector ] >>= fun () ->
    return ()

  let is_initialised device sector =
    let open Lwt_read_error in
    (* check for magic, initialise if not found *)
    B.read device sector_signature [ sector ] >>= fun () ->
    let magic' = Bytes.make (String.length magic) '\000' in
    Cstruct.blit_to_bytes sector 0 magic' 0 (Bytes.length magic');
    return ((Bytes.of_string magic) = magic')

  let create device info sector =
    let open ResultM in
    if info.Mirage_block.size_sectors < minimum_size_sectors
    then Lwt.return (Error (`Msg (Printf.sprintf "The block device is too small for a ring; the minimum size is %Ld sectors" minimum_size_sectors)))
    else (Lwt_read_error.to_msg @@ is_initialised device sector) >>= function
      | true -> return ()
      | false -> Lwt_write_error.to_msg @@ initialise device sector

  let read sector_offset device sector =
    Lwt_read_error.to_msg @@ B.read device sector_offset [ sector ]

  let write sector_offset device sector =
    Lwt_write_error.to_msg @@ B.write device sector_offset [ sector ]

  type producer = {
    queue: string;
    client: string;
    producer: int64;
    suspend_ack: bool;
  }

  type consumer = {
    queue: string;
    client: string;
    consumer: int64;
    suspend: bool;
  }

  let debug_info device sector =
    Lwt_read_error.to_msg @@ B.read device sector_producer [ sector ] >>= fun () ->
    let producer = Cstruct.LE.get_uint64 sector 0 in
    let suspend_ack = Cstruct.get_uint8 sector 8 in
    Lwt_read_error.to_msg @@ B.read device sector_consumer [ sector ] >>= fun () ->
    let consumer = Cstruct.LE.get_uint64 sector 0 in
    let suspend = Cstruct.get_uint8 sector 8 in
    return [
      "producer/offset", Int64.to_string producer;
      "producer/suspend_ack", string_of_int suspend_ack;
      "consumer/offset", Int64.to_string consumer;
      "consumer/suspend", string_of_int suspend
    ]

  let get_producer ?(queue="") ?(client="") device sector =
    Lwt_read_error.to_msg @@ B.read device sector_producer [ sector ] >>= fun () ->
    let producer = Cstruct.LE.get_uint64 sector 0 in
    Lwt.return (bool_of_int (Cstruct.get_uint8 sector 8))
    >>= fun suspend_ack ->
    trace [ `Get(client, queue, `Producer, `Int64 producer); `Get(client, queue, `Suspend_ack, `Bool suspend_ack) ]
    >>= fun () ->
    return { queue; client; producer; suspend_ack }

  let set_producer ?(queue="") ?(client="") device sector v =
    zero sector;
    Cstruct.LE.set_uint64 sector 0 v.producer;
    Cstruct.set_uint8 sector 8 (int_of_bool v.suspend_ack);
    (* Add human-readable debug into spare space in the sector *)
    let msg = Printf.sprintf "%s used by %s; producer = %Ld; suspend_ack = %b" v.queue v.client v.producer v.suspend_ack in
    Cstruct.blit_from_string msg 0 sector 128 (min (512 - 128) (String.length msg));
    trace [ `Set(client, queue, `Producer, `Int64 v.producer); `Set(client, queue, `Suspend_ack, `Bool v.suspend_ack) ]
    >>= fun () ->
    Lwt_write_error.to_msg @@ B.write device sector_producer [ sector ] >>= fun () ->
    return ()

  let get_consumer ?(queue="") ?(client="") device sector =
    Lwt_read_error.to_msg @@ B.read device sector_consumer [ sector ] >>= fun () ->
    let consumer = Cstruct.LE.get_uint64 sector 0 in
    Lwt.return (bool_of_int (Cstruct.get_uint8 sector 8))
    >>= fun suspend ->
    trace [ `Get(client, queue, `Consumer, `Int64 consumer); `Get(client, queue, `Suspend, `Bool suspend) ]
    >>= fun () ->
    return { queue; client; consumer; suspend }

  let set_consumer ?(queue="") ?(client="") device sector v =
    zero sector;
    Cstruct.LE.set_uint64 sector 0 v.consumer;
    Cstruct.set_uint8 sector 8 (int_of_bool v.suspend);
    (* Add human-readable debug into spare space in the sector *)
    let msg = Printf.sprintf "%s used by %s; consumer = %Ld; suspend = %b" v.queue v.client v.consumer v.suspend in
    Cstruct.blit_from_string msg 0 sector 128 (min (512 - 128) (String.length msg));
    trace [ `Set(client, queue, `Consumer, `Int64 v.consumer); `Set(client, queue, `Suspend, `Bool v.suspend) ]
    >>= fun () ->
    Lwt_write_error.to_msg @@ B.write device sector_consumer [ sector ]

  let get_data_sectors info = Int64.(sub info.Mirage_block.size_sectors sector_data)

  let get_sector_and_offset info byte_offset =
    let sector = Int64.(div byte_offset (of_int info.Mirage_block.sector_size)) in
    let offset = Int64.(to_int (rem byte_offset (of_int info.Mirage_block.sector_size))) in
    (sector,offset)

  type position = int64 [@@deriving sexp_of]

  let compare (a: position) (b: position) =
    if a < b then `LessThan
    else if a > b then `GreaterThan
    else `Equal

end

module Make(Log: S.LOG)(B: S.BLOCK)(Item: S.CSTRUCTABLE) = struct

module Producer = struct
  module C = Common(Log)(B)

  type position = C.position [@@deriving sexp_of]
  let compare = C.compare
  type item = Item.t
  type error = C.error
  let pp_error = C.pp_error
  let open_error = C.open_error
  let error_to_msg = C.error_to_msg
  type 'a result = 'a C.result

  type t = {
    disk: B.t;
    info: Mirage_block.info;
    mutable producer: C.producer; (* cache of the last value we wrote *)
    mutable attached: bool;
    queue: string;
    client: string;
  }

  let producer_m = Lwt_mutex.create ()
  let with_lock (f: unit -> 'a) = Lwt_mutex.with_lock producer_m f

  let create ~disk:disk () = with_lock @@ fun () ->
    B.get_info disk >>= fun info ->
    let open C in
    let sector = alloc info.Mirage_block.sector_size in
    create disk info sector

  let detach t = with_lock @@ fun () ->
    t.attached <- false;
    return ()

  let must_be_attached t f =
    if not t.attached
    then return (Error (`Msg "Ring has been detached and cannot be used"))
    else f ()

  let ok_to_write t needed_bytes =
    let client, queue = t.client, t.queue in
    let open C in
    let open ResultM in
    let sector = alloc t.info.Mirage_block.sector_size in
    get_consumer ~client ~queue t.disk sector >>= fun c ->
    ( if c.suspend <> t.producer.suspend_ack then begin
        let producer = { t.producer with suspend_ack = c.suspend } in
        set_producer ~client ~queue t.disk sector producer >>= fun () ->
        t.producer <- producer;
        return ()
      end else return () ) >>= fun () ->
    if c.suspend
    then Lwt.return (Error `Suspended)
    else
      let used = Int64.sub t.producer.producer c.consumer in
      let total = Int64.(mul (of_int t.info.Mirage_block.sector_size) (C.get_data_sectors t.info)) in
      let total_sectors = get_data_sectors t.info in
      if Int64.(mul total_sectors (of_int t.info.Mirage_block.sector_size)) < needed_bytes
      then Lwt.return (Error (`Msg (Printf.sprintf "The ring is too small for a message of size %Ld bytes" needed_bytes)))
      else if Int64.sub total used < needed_bytes
      then Lwt.return (Error `Retry)
      else return ()

  let attach ?(queue="unknown") ?(client="unknown") ~disk:disk () = with_lock @@ fun () ->
    B.get_info disk >>= fun info ->
    let open C in
    let open ResultM in
    let sector = alloc info.Mirage_block.sector_size in
    Lwt_read_error.to_msg @@ is_initialised disk sector >>= function
    | false -> Lwt.return (Error (`Msg "block ring has not been initialised"))
    | true ->
      get_producer ~queue ~client disk sector >>= fun producer ->
      let t = {
        disk;
        info;
        producer;
        attached = true;
        queue;
        client
        } in
      (* acknowledge any pending suspend/resume from the consumer *)
      ok_to_write t 0L
      >>= fun _ ->
      return t

  let state t = with_lock @@ fun () ->
    let open Lwt in
    ok_to_write t 0L
    >>= function
    | Error `Suspended -> return (Ok `Suspended)
    | Error `Retry -> return (Error (`Msg "Internal error: it should always be ok to write 0 bytes"))
    | Error x -> return (Error x)
    | Ok () -> return (Ok `Running)

  let read_modify_write t offset fn =
    let open C in
    let open ResultM in
    let sector = alloc t.info.Mirage_block.sector_size in
    let total_sectors = get_data_sectors t.info in
    let realsector = Int64.(add sector_data (rem offset total_sectors)) in
    read realsector t.disk sector >>= fun () ->
    let result = fn sector in
    write realsector t.disk sector >>= fun () ->
    return result

  let unsafe_write (t:t) item =
    let open C in
    let open ResultM in
    let _sector = alloc t.info.Mirage_block.sector_size in
    (* add a 4 byte header of size, and round up to the next 4-byte offset *)
    let needed_bytes = Int64.(logand (lognot 3L) (add 7L (of_int (Cstruct.len item)))) in
    let first_sector = Int64.(div t.producer.producer (of_int t.info.Mirage_block.sector_size)) in
    let first_offset = Int64.(to_int (rem t.producer.producer (of_int t.info.Mirage_block.sector_size))) in

    (* Do first sector. We know that we'll always be able to fit in the length header into
       the first page as it's only a 4-byte integer and we're padding to 4-byte offsets. *)
    read_modify_write t first_sector (fun sector ->
      (* Write the header and anything else we can *)
      Cstruct.LE.set_uint32 sector first_offset (Int32.of_int (Cstruct.len item));
      if first_offset + 4 = t.info.Mirage_block.sector_size
      then item (* We can't write anything else, so just return the item *)
      else begin
        let this = min (t.info.Mirage_block.sector_size - first_offset - 4) (Cstruct.len item) in
        Cstruct.blit item 0 sector (first_offset + 4) this;
        Cstruct.shift item this
      end) >>= fun remaining ->

    let rec loop offset remaining =
      if Cstruct.len remaining = 0
      then return ()
      else begin
        read_modify_write t offset (fun sector ->
          let this = min t.info.Mirage_block.sector_size (Cstruct.len remaining) in
          let frag = Cstruct.sub sector 0 this in
          Cstruct.blit remaining 0 frag 0 (Cstruct.len frag);
          Cstruct.shift remaining this) >>= fun remaining ->
        loop (Int64.succ offset) remaining
      end in
    loop (Int64.succ first_sector) remaining >>= fun () ->
      (* Write the payload before updating the producer pointer *)
    let new_producer = Int64.add t.producer.producer needed_bytes in
    return new_producer

  let advance ~t ~position:new_producer () = with_lock (fun () ->
    must_be_attached t
      (fun () ->
        let open C in
        let open ResultM in
        let sector = alloc t.info.Mirage_block.sector_size in
        let producer = { t.producer with producer = new_producer } in
        set_producer ~queue:t.queue ~client:t.client t.disk sector producer >>= fun () ->
        t.producer <- producer;
        return ()
      )
  )

  let push ~t ~item () = with_lock (fun () ->
    must_be_attached t
      (fun () ->
        let item = Item.to_cstruct item in
        (* every item has a 4 byte header *)
        let needed_bytes = Int64.(add 4L (of_int (Cstruct.len item))) in
        let open C in
        let open ResultM in
        ok_to_write t needed_bytes
        >>= fun () ->
        unsafe_write t item
      )
  )

  let debug_info t = with_lock (fun () ->
    let sector = alloc t.info.Mirage_block.sector_size in
    C.debug_info t.disk sector
  )

end

module Consumer = struct
  module C = Common(Log)(B)

  type position = C.position [@@deriving sexp_of]
  let compare = C.compare
  type item = Item.t
  type error = C.error
  let pp_error = C.pp_error
  let open_error = C.open_error
  let error_to_msg = C.error_to_msg
  type 'a result = 'a C.result

  type t = {
    disk: B.t;
    info: Mirage_block.info;
    mutable consumer: C.consumer; (* cache of the last value we wrote *)
    mutable attached: bool;
    queue: string;
    client: string;
  }

  let consumer_m = Lwt_mutex.create ()
  let with_lock (f: unit -> 'a) = Lwt_mutex.with_lock consumer_m f

  let detach t = with_lock @@ fun () ->
    t.attached <- false;
    return ()

  let must_be_attached t f =
    if not t.attached
    then return (Error (`Msg "Ring has been detached and cannot be used"))
    else f ()

  let attach ?(queue="unknown") ?(client="unknown") ~disk:disk () = with_lock @@ fun () ->
    let open Lwt in
    B.get_info disk >>= fun info ->
    let open C in
    let open ResultM in
    let sector = alloc info.Mirage_block.sector_size in
    Lwt_read_error.to_msg @@ is_initialised disk sector >>= function
    | false -> Lwt.return (Error (`Msg "block ring has not been initialised"))
    | true ->
      get_consumer ~queue ~client disk sector >>= fun consumer ->
      return {
        disk;
        info;
        consumer;
        attached = true;
        queue;
        client;
      }

  let suspend (t:t) = with_lock @@ fun () ->
    let client, queue = t.client, t.queue in
    let open C in
    let open ResultM in
    let sector = alloc t.info.Mirage_block.sector_size in
    get_producer ~client:client ~queue:queue t.disk sector
    >>= fun producer ->
    if t.consumer.C.suspend <> producer.C.suspend_ack
    then Lwt.return (Error `Retry)
    else begin
      let consumer = { t.consumer with C.suspend = true } in
      C.set_consumer ~queue:queue ~client:client t.disk sector consumer
      >>= fun () ->
      t.consumer <- consumer;
      return ()
    end

  let state t = with_lock @@ fun () ->
    let client, queue = t.client, t.queue in
    let open C in
    let open ResultM in
    let sector = alloc t.info.Mirage_block.sector_size in
    C.get_producer ~client ~queue t.disk sector
    >>= fun p ->
    return (if p.C.suspend_ack then `Suspended else `Running)

  let resume (t: t) = with_lock @@ fun () ->
    let open C in
    let open ResultM in
    let sector = alloc t.info.Mirage_block.sector_size in
    C.get_producer ~client:t.client ~queue:t.queue t.disk sector
    >>= fun producer ->
    if t.consumer.C.suspend <> producer.C.suspend_ack
    then Lwt.return (Error `Retry)
    else
      let consumer = { t.consumer with C.suspend = false } in
      C.set_consumer ~queue:t.queue ~client:t.client t.disk sector consumer
      >>= fun () ->
      t.consumer <- consumer;
      return ()

  let pop t = with_lock @@ fun () ->
    let open C in
    let open ResultM in
    let sector = alloc t.info.Mirage_block.sector_size in
    let total_sectors = get_data_sectors t.info in
    get_producer ~client:t.client ~queue:t.queue t.disk sector >>= fun producer ->
    let available_bytes = Int64.sub producer.producer t.consumer.consumer in
    if available_bytes <= 0L
    then Lwt.return (Error `Retry)
    else begin
      let first_sector,first_offset = get_sector_and_offset t.info t.consumer.consumer in
      read Int64.(add sector_data (rem first_sector total_sectors)) t.disk sector >>= fun () ->
      let len = Int32.to_int (Cstruct.LE.get_uint32 sector first_offset) in
      let result = Cstruct.create len in
      let this = min len (t.info.Mirage_block.sector_size - first_offset - 4) in
      let frag = Cstruct.sub sector (4 + first_offset) this in
      Cstruct.blit frag 0 result 0 this;
      let rec loop consumer remaining =
        if Cstruct.len remaining = 0
        then return ()
        else
          let this = min t.info.Mirage_block.sector_size (Cstruct.len remaining) in
          let frag = Cstruct.sub remaining 0 this in
          read Int64.(add sector_data (rem consumer total_sectors)) t.disk sector >>= fun () ->
          Cstruct.blit sector 0 frag 0 this;
          loop (Int64.succ consumer) (Cstruct.shift remaining this) in
      loop (Int64.succ first_sector) (Cstruct.shift result this) >>= fun () ->
      (* Read the payload before updating the consumer pointer *)
      let needed_bytes = Int64.(logand (lognot 3L) (add 7L (of_int (len)))) in
      match Item.of_cstruct result with
      | None -> Lwt.return (Error (`Msg (Printf.sprintf "Failed to parse queue item: (%d)[%s]" (Cstruct.len result) (String.escaped (Cstruct.to_string result)))))
      | Some result ->
        return (Int64.(add t.consumer.consumer needed_bytes),result)
    end

  let pop ~t ?(from = t.consumer.C.consumer) () =
    must_be_attached t
      (fun () ->
        pop { t with consumer = { t.consumer with C.consumer = from } }
      )

  let rec fold ~f ~t ?(from = t.consumer.C.consumer) ~init:acc () =
    let open Lwt in
    pop ~t ~from ()
    >>= function
    | Error `Retry -> return (Ok (from, acc))
    | Error x -> return (Error x)
    | Ok (from, x) -> fold ~f ~t ~from ~init:(f x acc) ()

  let advance ~t ~position:consumer () = with_lock @@ fun () ->
    must_be_attached t
      (fun () ->
        let open C in
        let open ResultM in
        let sector = alloc t.info.Mirage_block.sector_size in
        let consumer' = { t.consumer with consumer = consumer } in
        set_consumer ~queue:t.queue ~client:t.client t.disk sector consumer' >>= fun () ->
        t.consumer <- consumer';
        return ()
      )

  let debug_info t = with_lock @@ fun () ->
    let sector = alloc t.info.Mirage_block.sector_size in
    C.debug_info t.disk sector
end
end
