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

module Make
  (Log: S.LOG)
  (Block: S.BLOCK)
  (Time: S.TIME)
  (Clock: S.CLOCK)
  (Operation: S.CSTRUCTABLE):
  S.JOURNAL
  with type disk := Block.t
   and type operation := Operation.t
(** Create a journal from a block device. Descriptions of idempotent operations
    may be pushed to the journal, and we guarantee to perform them at-least-once
    in the order they were pushed provided the block device is available. If
    the program crashes, the journal replay will be started when the program
    restarts. *)
