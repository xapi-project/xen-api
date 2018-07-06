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

(** A producer/consumer ring on top of a shared block device. The producer may
    push variable-sized items (if there is enough space) and the consumer may
    then pop the items. Items are pushed and popped atomically. There should
    be at-most-one producer and at-most-one consumer at any point in time.
    Since block devices have no built-in signalling mechanisms, it is up to
    the client to either poll for updates or implement another out-of-band
    signalling mechanism. *)

module Make(Log: S.LOG)(B: S.BLOCK)(Item: S.CSTRUCTABLE): sig

  module Producer: S.PRODUCER
    with type disk := B.t
     and type item = Item.t

  module Consumer: S.CONSUMER
    with type disk := B.t
     and type position = Producer.position
     and type item = Item.t
end
