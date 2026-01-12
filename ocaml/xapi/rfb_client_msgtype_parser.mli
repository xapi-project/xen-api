(*
 * Copyright (C) Cloud Software Group
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

(** RFB (Remote Framebuffer) Protocol Parser for VNC Console Connections
    
    This module provides a stateful parser only for RFB client-to-server messages,
    aiming to identify message types from clients.
*)

type msg =
  | Handshake
  | SetPixelFormat
  | SetEncodings
  | FramebufferUpdateRequest
  | KeyEvent
  | PointerEvent
  | ClientCutText
  | QEMUClientMessage

val string_of_msg : msg -> string
(** Convert a message type to its string representation *)

val create : unit -> string -> (msg list, string) result
(** Create a new RFB parser instance.
    
    Returns a function that processes incoming data chunks and returns
    either a list of parsed messages or an error string.
    
    Usage:
    {[
      let parser = create () in
      match parser data_chunk with
      | Ok messages -> 
          List.iter (fun msg -> 
            Printf.printf "Parsed: %s\n" (string_of_msg msg)
          ) messages
      | Error error_msg -> 
          Printf.printf "Parse error: %s\n" error_msg
    ]}
    
   
    Supported message types:
    - Handshake - Successful RFB handshake (ProtocolVersion + ClientInit)
    - SetPixelFormat - Set pixel format (20 bytes)
    - SetEncodings - Set encodings (variable length)
    - FramebufferUpdateRequest - Request framebuffer update (10 bytes)
    - KeyEvent - Keyboard input (8 bytes)
    - PointerEvent - Mouse/pointer input (6 bytes)
    - ClientCutText - Clipboard text (variable length)
    - QEMUClientMessage - QEMU-specific message (12 bytes)
    
    Error handling:
    - Returns Error "BadHandshake: ..." for failed handshake
    - Returns Error "UnsupportedMsg: ..." for unrecognized message types
    - Returns Error "Parse error: ..." for malformed data
    
    @return function of type (string -> (msg list, string) result) that processes data chunks
*)
