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

open Angstrom

module D = Debug.Make (struct let name = "rfb_parser" end)

open D

module RfbParser = struct
  type ok_msg =
    | Handshake
    | SetPixelFormat
    | SetEncodings
    | FramebufferUpdateRequest
    | KeyEvent
    | PointerEvent
    | ClientCutText
    | QEMUClientMessage

  type unsupported =
    | BadHandshake of string  (** Failed/unsupported handshake with raw data *)
    | UnknownMsg of string  (** Unrecognized message type with raw data *)

  type msg_type = Ok of ok_msg | Unsupported of unsupported | Fail

  (* Helper function to format binary data as hex string, up to max_bytes *)
  let hex_dump_data data max_bytes =
    let len = min (String.length data) max_bytes in
    let hex_str =
      String.fold_left
        (fun acc c ->
          let hex_byte = Printf.sprintf "%02x" (Char.code c) in
          if acc = "" then hex_byte else acc ^ " " ^ hex_byte
        )
        "" (String.sub data 0 len)
    in
    if String.length data > max_bytes then
      hex_str ^ "..."
    else
      hex_str

  let string_of_msg = function
    | Ok Handshake ->
        "Handshake"
    | Ok SetPixelFormat ->
        "SetPixelFormat"
    | Ok SetEncodings ->
        "SetEncodings"
    | Ok FramebufferUpdateRequest ->
        "FramebufferUpdateRequest"
    | Ok KeyEvent ->
        "KeyEvent"
    | Ok PointerEvent ->
        "PointerEvent"
    | Ok ClientCutText ->
        "ClientCutText"
    | Ok QEMUClientMessage ->
        "QEMUClientMessage"
    | Unsupported (BadHandshake _) ->
        "BadHandshake"
    | Unsupported (UnknownMsg _) ->
        "UnknownMsg"
    | Fail ->
        "Fail"

  (*
    https://github.com/rfbproto/rfbproto/blob/master/rfbproto.rst#client-to-server-messages
    Currently, only supports RFB version 3.3, and parses the most common used messages.
    Can be extended to support more message types as needed.
  *)

  (* Parse ProtocolVersion: "RFB 003.003\n" (12 bytes) *)
  let parse_protocol_version =
    take 12 >>= fun data ->
    if data = "RFB 003.003\n" then
      return (true, "")
    else
      return (false, data)

  (* Parse ClientInit: 1 byte shared-flag (0 or 1) *)
  let parse_client_init =
    take 1 >>= fun data ->
    let shared_flag = Char.code data.[0] in
    if shared_flag = 0 || shared_flag = 1 then
      return (true, "")
    else
      return (false, data)

  (* Combine protocol version and client init parsers *)
  let parse_handshake =
    both parse_protocol_version parse_client_init
    >>= fun ((proto_ok, proto_data), (init_ok, init_data)) ->
    if proto_ok && init_ok then
      return (Ok Handshake)
    else
      let failed_data = proto_data ^ init_data in
      return (Unsupported (BadHandshake failed_data))

  (* Parse SetPixelFormat: message-type(1) + padding(3) + pixel-format(16) = 20 bytes *)
  let parse_set_pixel_format =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 0 then
      take 19 >>| fun _ -> Ok SetPixelFormat
    else
      fail "Not SetPixelFormat"

  (* Parse SetEncodings: message-type(1) + padding(1) + num-encodings(2) + encodings(4*n) *)
  let parse_set_encodings =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 2 then
      take 1 >>= fun _ ->
      Angstrom.BE.any_uint16 >>= fun num_encodings ->
      take (num_encodings * 4) >>| fun _ -> Ok SetEncodings
    else
      fail "Not SetEncodings"

  (* Parse FramebufferUpdateRequest: message-type(1) + incremental(1) + x(2) + y(2) + width(2) + height(2) = 10 bytes *)
  let parse_framebuffer_update_request =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 3 then
      take 9 >>| fun _ -> Ok FramebufferUpdateRequest
    else
      fail "Not FramebufferUpdateRequest"

  (* Parse KeyEvent: message-type(1) + down-flag(1) + padding(2) + key(4) = 8 bytes *)
  let parse_key_event =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 4 then
      take 7 >>| fun _ -> Ok KeyEvent
    else
      fail "Not KeyEvent"

  (* Parse PointerEvent: message-type(1) + button-mask(1) + x(2) + y(2) = 6 bytes *)
  let parse_pointer_event =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 5 then
      take 5 >>| fun _ -> Ok PointerEvent
    else
      fail "Not PointerEvent"

  (* Parse ClientCutText: message-type(1) + padding(3) + length(4) + text(length) *)
  let parse_client_cut_text =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 6 then
      take 3 >>= fun _ ->
      Angstrom.BE.any_int32 >>= fun text_length ->
      take (Int32.to_int text_length) >>| fun _ -> Ok ClientCutText
    else
      fail "Not ClientCutText"

  (* Parse QEMU Client Message: message-type(1) + submessage-type(1) + data(10) = 12 bytes *)
  let parse_qemu_client_message =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 255 then
      take 11 >>| fun _ -> Ok QEMUClientMessage
    else
      fail "Not QEMUClientMessage"

  (* Fallback parser for unknown messages *)
  let parse_unsupported_message =
    take 1 >>= fun msg_type -> return (Unsupported (UnknownMsg msg_type))

  let parse_rfb_message handshake_completed =
    if not handshake_completed then
      parse_handshake
    else
      choice
        [
          parse_set_pixel_format
        ; parse_set_encodings
        ; parse_framebuffer_update_request
        ; parse_key_event
        ; parse_pointer_event
        ; parse_client_cut_text
        ; parse_qemu_client_message
        ; parse_unsupported_message (* Fallback *)
        ]

  (* Create RFB parser with closure-based state encapsulation *)
  (* An example usage of the parser:
     let rfb_data_callback () =
     let rfb_parser = create () in
     fun data ->
       try
         let parsed_messages = rfb_parser data in
         Handle_messages
       with exn ->
         Error_handler
  *)
  let create () =
    let module State = struct
      type t = {
          parser: msg_type Angstrom.Buffered.state
        ; handshake_completed: bool
      }
    end in
    (* Private state hidden in closure *)
    let state =
      ref
        {
          State.parser= Angstrom.Buffered.parse (parse_rfb_message false)
        ; handshake_completed= false
        }
    in

    (* Helper to get unconsumed data as string *)
    let unconsumed_to_string {Angstrom.Buffered.buf; off; len} =
      Bigstringaf.substring buf ~off ~len
    in

    (* Update handshake completion based on parsed message *)
    let update_handshake_state current_state message_type =
      match message_type with Ok Handshake -> true | _ -> current_state
    in

    let rec process_parser parser messages =
      match parser with
      | Angstrom.Buffered.Done (unconsumed, message_type) ->
          let new_messages = message_type :: messages in
          let has_handshake_completed =
            update_handshake_state !state.State.handshake_completed message_type
          in

          if match message_type with Unsupported _ -> true | _ -> false then (
            let data_info =
              match message_type with
              | Unsupported (BadHandshake data) ->
                  Printf.sprintf "BadHandshake with data: %s"
                    (hex_dump_data data 20)
              | Unsupported (UnknownMsg data) ->
                  Printf.sprintf "UnknownMsg with data: %s"
                    (hex_dump_data data 20)
              | _ ->
                  "Unsupported message"
            in
            debug "Stopping RFB parsing due to unsupported message: %s"
              data_info ;
            (* Return unsupported message to caller to stop parsing *)
            (parser, !state.State.handshake_completed, new_messages)
          ) else if unconsumed.len > 0 then
            let next_parser =
              Angstrom.Buffered.parse (parse_rfb_message has_handshake_completed)
            in
            let unconsumed_str = unconsumed_to_string unconsumed in
            let fed_parser =
              Angstrom.Buffered.feed next_parser (`String unconsumed_str)
            in
            process_parser fed_parser new_messages
          else
            let fresh_parser =
              Angstrom.Buffered.parse (parse_rfb_message has_handshake_completed)
            in
            (fresh_parser, has_handshake_completed, new_messages)
      | Angstrom.Buffered.Partial _ ->
          (parser, !state.State.handshake_completed, messages)
      | Angstrom.Buffered.Fail (unconsumed, _, error_msg) ->
          (* Generate a Fail message for caller to stop parsing *)
          debug "RFB parser failed: %s, unconsumed data (%d bytes): %s"
            error_msg unconsumed.len
            (hex_dump_data (unconsumed_to_string unconsumed) 20) ;
          let fail_messages = Fail :: messages in
          (parser, !state.State.handshake_completed, fail_messages)
    in

    let on_data data_chunk =
      let new_parser =
        Angstrom.Buffered.feed !state.State.parser (`String data_chunk)
      in
      let final_parser, final_handshake_completed, new_messages =
        process_parser new_parser []
      in
      state :=
        {
          State.parser= final_parser
        ; handshake_completed= final_handshake_completed
        } ;
      List.rev_map string_of_msg new_messages
    in

    (* Return the data processing function *)
    on_data
end
