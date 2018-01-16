(*
 * Copyright (C) 2006-2009 Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)
open Rfb

let w = 640
let h = 480

let server (s: Unix.file_descr) =
  handshake w h s;

  let started = ref false in
  let bpp = ref 32 in

  while true do
    let req = Request.unmarshal s in
    print_endline (Request.prettyprint req);
    match req with
    | Request.SetPixelFormat pf ->
      bpp := pf.PixelFormat.bpp;
    | Request.FrameBufferUpdateRequest _ ->
      if not(!started) then begin
        (* Update the whole thing *)
        let buffer = Bytes.create (w * h * !bpp / 8) in
        for i = 0 to String.length buffer - 1 do
          buffer.[i] <- char_of_int (Random.int 255)
        done;
        let raw = { FramebufferUpdate.Raw.buffer = buffer } in
        let update = { FramebufferUpdate.x = 0; y = 0; w = w; h = h;
                       encoding = FramebufferUpdate.Encoding.Raw raw } in
        really_write s (FramebufferUpdate.marshal [ update ]);
        started := true;
      end else begin
        (* send a copyrect *)
        let w' = Random.int w and h' = Random.int h in
        let x' = Random.int (w - w') and y' = Random.int (h - h') in
        let x'' = Random.int (w - w') and y'' = Random.int (h - h') in
        let cr = { FramebufferUpdate.CopyRect.x = x''; y = y'' } in
        let update = { FramebufferUpdate.x = x'; y = y'; w = w'; h = h';
                       encoding = FramebufferUpdate.Encoding.CopyRect cr } in
        really_write s (FramebufferUpdate.marshal [ update ])
      end
    | _ -> ()
  done


