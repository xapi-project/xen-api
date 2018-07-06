(*
 * Copyright (c) 2011-2015 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013-2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2013      Citrix Systems Inc
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

type error = Mirage_device.error
let pp_error = Mirage_device.pp_error

type stats = {
  mutable rx_bytes: int64;
  mutable rx_pkts: int32;
  mutable tx_bytes: int64;
  mutable tx_pkts: int32;
}

module Stats = struct
  let create () = { rx_pkts=0l; rx_bytes=0L; tx_pkts=0l; tx_bytes=0L }

  let rx t size =
    t.rx_pkts <- Int32.succ t.rx_pkts;
    t.rx_bytes <- Int64.add t.rx_bytes size

  let tx t size =
    t.tx_pkts <- Int32.succ t.tx_pkts;
    t.tx_bytes <- Int64.add t.tx_bytes size

  let reset t =
    t.rx_bytes <- 0L;
    t.rx_pkts  <- 0l;
    t.tx_bytes <- 0L;
    t.tx_pkts  <- 0l
end

module type S = sig
  type error = private [> Mirage_device.error]
  val pp_error: error Fmt.t
  type page_aligned_buffer
  type buffer
  type macaddr
  include Mirage_device.S
  val write: t -> buffer -> (unit, error) result io
  val writev: t -> buffer list -> (unit, error) result io
  val listen: t -> (buffer -> unit io) -> (unit, error) result io
  val mac: t -> macaddr
  val get_stats_counters: t -> stats
  val reset_stats_counters: t -> unit
end
