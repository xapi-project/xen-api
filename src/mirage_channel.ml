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

(** MirageOS signature for channel devices.

    Channels are buffered byte-streams which are attached to an
    unbuffered flow (e.g. a TCPv4 connection).

    {e Release %%VERSION%% } *)

open Result

module type S = sig

  type error
  (** The type for errors. *)

  val pp_error: error Fmt.t
  (** [pp_error] is the pretty-printer for errors. *)

  type write_error = private [> Mirage_flow.write_error]
  (** The type for write errors. *)

  val pp_write_error: write_error Fmt.t
  (** [pp_write_error] is the pretty-printer for write errors. *)

  type buffer
    (** The type for memory buffers. *)

  type flow
  (** The type for unbuffered network flow. *)

  type t
  (** The type for the state associated with channels, such as the
      inflight buffers. *)

  type +'a io
  (** The type for potentially blocking I/O operation *)

  val create: flow -> t
  (** [create flow] allocates send and receive buffers and
      associates them with the given unbuffered [flow]. *)

  val to_flow: t -> flow
  (** [to_flow t] returns the flow that backs this channel. *)

  val read_char: t -> (char Mirage_flow.or_eof, error) result io
  (** Reads a single character from the channel, blocking if there is
      no immediately available input data. *)

  val read_some: ?len:int -> t -> (buffer Mirage_flow.or_eof, error) result io
  (** [read_some ?len t] reads up to [len] characters from the
      input channel and at most a full [buffer]. If [len] is not
      specified, it reads all available data and returns that
      buffer. *)

  val read_exactly: len:int -> t -> (buffer list Mirage_flow.or_eof, error) result io
  (** [read_exactly len t] reads [len] bytes from the channel [t] or fails
      with [Eof]. *)

  val read_line: t -> (buffer list Mirage_flow.or_eof, error) result io
  (** [read_line t] reads a line of input, which is terminated
      either by a CRLF sequence, or the end of the channel (which
      counts as a line).  @return Returns a list of views that
      terminates at EOF. *)

  val write_char: t -> char -> unit
  (** [write_char t ch] writes a single character to the output
      channel. *)

  val write_string: t -> string -> int -> int -> unit
  (** [write_string t buf off len] writes [len] bytes from a string
      [buf], starting from from offset [off]. *)

  val write_buffer: t -> buffer -> unit
  (** [write_buffer t buf] copies the buffer to the channel's
      output buffer.  The buffer should not be modified after being
      written, and it will be recycled into the buffer allocation pool
      at some future point. *)

  val write_line: t -> string -> unit
  (** [write_line t buf] writes the string [buf] to the output
      channel and append a newline character afterwards. *)

  val flush: t -> (unit, write_error) result io
  (** [flush t] flushes the output buffer and block if necessary
      until it is all written out to the flow. *)

  val close: t -> (unit, write_error) result io
  (** [close t] calls {!flush} and then close the underlying
      flow. *)

end
