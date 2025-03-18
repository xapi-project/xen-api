(*
 * Copyright (C) 2016 David Scott <dave.scott@unikernel.com>
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
 *
 *)
open Lwt.Infix

module Lwt_error = struct
  open Lwt.Infix
  module Infix = struct
    let ( >>= ) m f = m >>= function
      | Ok x -> f x
      | Error `Disconnected -> Lwt.fail_with "Disconnected"
      | Error _ -> Lwt.fail_with "Unknown error"
  end
end

module Lwt_write_error = struct
  module Infix = struct
    open Lwt.Infix
    let ( >>= ) m f = m >>= function
      | Ok x -> f x
      | Error `Is_read_only -> Lwt.fail_with "Is_read_only"
      | Error `Disconnected -> Lwt.fail_with "Disconnected"
      | Error _ -> Lwt.fail_with "Unknown error"
  end
end

module Infix = struct
  let (>>=) m f = m >>= function
    | Error e -> Lwt.return (Error e)
    | Ok x -> f x
end

module FromResult = struct
  let (>>=) m f = match m with
    | Result.Error x -> Lwt.return (Error x)
    | Result.Ok x -> f x
end
