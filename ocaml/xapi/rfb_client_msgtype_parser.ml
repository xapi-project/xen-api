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

type msg =
  | Handshake
  | SetPixelFormat
  | SetEncodings
  | FramebufferUpdateRequest
  | KeyEvent
  | PointerEvent
  | ClientCutText
  | QEMUClientMessage

(* Helper function to format binary data as hex string, up to max_bytes *)
let hex_dump_data data max_bytes =
  let len = min (String.length data) max_bytes in
  let hex_chars = ref [] in
  for i = 0 to len - 1 do
    hex_chars := Printf.sprintf "%02x" (Char.code data.[i]) :: !hex_chars
  done ;
  let hex_str = String.concat " " (List.rev !hex_chars) in
  if String.length data > max_bytes then
    String.concat "" [hex_str; "..."]
  else
    hex_str

let string_of_msg = function
  | Handshake ->
      "Handshake"
  | SetPixelFormat ->
      "SetPixelFormat"
  | SetEncodings ->
      "SetEncodings"
  | FramebufferUpdateRequest ->
      "FramebufferUpdateRequest"
  | KeyEvent ->
      "KeyEvent"
  | PointerEvent ->
      "PointerEvent"
  | ClientCutText ->
      "ClientCutText"
  | QEMUClientMessage ->
      "QEMUClientMessage"

(*
    https://github.com/rfbproto/rfbproto/blob/master/rfbproto.rst#client-to-server-messages
    Currently, only supports RFB version 3.3, and parses the most common used messages.
    Can be extended to support more message types as needed.
  *)

(* Parse ProtocolVersion: "RFB 003.003\n" (12 bytes) *)
let parse_protocol_version =
  take 12 >>= fun data ->
  if data = "RFB 003.003\n" then
    return (Ok ())
  else
    let hex_data = hex_dump_data data 12 in
    return (Error hex_data)

(* Parse ClientInit: 1 byte shared-flag (0 or 1) *)
let parse_client_init =
  any_uint8 >>= fun shared_flag ->
  if shared_flag = 0 || shared_flag = 1 then
    return (Ok ())
  else
    let hex_data = Printf.sprintf "%02x" shared_flag in
    return (Error hex_data)

(* Combine protocol_version and client_init to one parser *)
let parse_handshake =
  both parse_protocol_version parse_client_init
  >>= fun (proto_result, init_result) ->
  match (proto_result, init_result) with
  | Ok (), Ok () ->
      return Handshake
  | Error proto_data, Ok () ->
      fail (String.concat "" ["BadHandshake: "; proto_data])
  | Ok (), Error init_data ->
      fail (String.concat "" ["BadHandshake: "; init_data])
  | Error proto_data, Error init_data ->
      let failed_data = String.concat "" [proto_data; init_data] in
      fail (String.concat "" ["BadHandshake: "; failed_data])

(* Parse SetPixelFormat: message-type(1) + padding(3) + pixel-format(16) = 20 bytes *)
let parse_set_pixel_format =
  any_uint8 >>= fun msg_type ->
  if msg_type = 0 then
    take 19 >>| fun _ -> SetPixelFormat
  else
    fail __FUNCTION__

(* Parse SetEncodings: message-type(1) + padding(1) + num-encodings(2) + encodings(4*n) *)
let parse_set_encodings =
  any_uint8 >>= fun msg_type ->
  if msg_type = 2 then
    take 1 >>= fun _ ->
    Angstrom.BE.any_uint16 >>= fun num_encodings ->
    take (num_encodings * 4) >>| fun _ -> SetEncodings
  else
    fail __FUNCTION__

(* Parse FramebufferUpdateRequest: message-type(1) + incremental(1) + x(2) + y(2) + width(2) + height(2) = 10 bytes *)
let parse_framebuffer_update_request =
  any_uint8 >>= fun msg_type ->
  if msg_type = 3 then
    take 9 >>| fun _ -> FramebufferUpdateRequest
  else
    fail __FUNCTION__

(* Parse KeyEvent: message-type(1) + down-flag(1) + padding(2) + key(4) = 8 bytes *)
let parse_key_event =
  any_uint8 >>= fun msg_type ->
  if msg_type = 4 then
    take 7 >>| fun _ -> KeyEvent
  else
    fail __FUNCTION__

(* Parse PointerEvent: message-type(1) + button-mask(1) + x(2) + y(2) = 6 bytes *)
let parse_pointer_event =
  any_uint8 >>= fun msg_type ->
  if msg_type = 5 then
    take 5 >>| fun _ -> PointerEvent
  else
    fail __FUNCTION__

(* Parse ClientCutText: message-type(1) + padding(3) + length(4) + text(length) *)
let parse_client_cut_text =
  any_uint8 >>= fun msg_type ->
  if msg_type = 6 then
    take 3 >>= fun _ ->
    Angstrom.BE.any_int32 >>= fun text_length ->
    take (Int32.to_int text_length) >>| fun _ -> ClientCutText
  else
    fail __FUNCTION__

(* Parse QEMU Client Message: message-type(1) + submessage-type(1) + data(10) = 12 bytes *)
let parse_qemu_client_message =
  any_uint8 >>= fun msg_type ->
  if msg_type = 255 then
    take 11 >>| fun _ -> QEMUClientMessage
  else
    fail __FUNCTION__

(* Fallback parser for unknown messages *)
let parse_unsupported_message =
  any_uint8 >>= fun msg_type ->
  let hex_data = Printf.sprintf "%02x" msg_type in
  (* Use commit to prevent backtracking, so we can fail and get the error message *)
  commit *> fail (String.concat "" ["UnsupportedMsg: "; hex_data])

let handshake_parser = parse_handshake

let message_parser =
  (* Put the most likely parsers first *)
  choice
    [
      parse_framebuffer_update_request
    ; parse_pointer_event
    ; parse_key_event
    ; parse_client_cut_text
    ; parse_qemu_client_message
    ; parse_set_pixel_format
    ; parse_set_encodings
    ; parse_unsupported_message (* Fallback *)
    ]

(* Create RFB parser with closure-based state encapsulation *)
let create () =
  (* Private state hidden in closure *)
  let state = ref (Angstrom.Buffered.parse handshake_parser) in

  (* Helper to get unconsumed data as string *)
  let unconsumed_to_string {Angstrom.Buffered.buf; off; len} =
    Bigstringaf.substring buf ~off ~len
  in

  let rec check_parsing_result acc =
    let open Angstrom.Buffered in
    Result.bind acc @@ fun (parser_state, msgs) ->
    match parser_state with
    | Done (unconsumed, parsed_result) ->
        let msgs = parsed_result :: msgs in
        if unconsumed.len > 0 then
          let new_parser = parse message_parser in
          let fed_parser =
            feed new_parser (`String (unconsumed_to_string unconsumed))
          in
          check_parsing_result (Ok (fed_parser, msgs))
        else
          Ok (parse message_parser, msgs)
    | Partial _ ->
        Ok (parser_state, msgs)
    | Fail (_unconsumed, _, error_msg) ->
        Error (String.concat "" ["Parse error: "; error_msg])
  in

  (* Return the data processing function *)
  fun data_chunk ->
    let new_parser = Angstrom.Buffered.feed !state (`String data_chunk) in
    check_parsing_result (Ok (new_parser, []))
    |> Result.map (fun (final_parser, messages) ->
        state := final_parser ;
        List.rev messages
    )
