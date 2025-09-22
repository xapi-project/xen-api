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

open Angstrom

module D = Debug.Make (struct let name = "rfb_parser" end)

open D

(* RFB Message Types *)
type rfb_message_type =
  | Handshake
  | SetPixelFormat
  | SetEncodings
  | FramebufferUpdateRequest
  | KeyEvent
  | PointerEvent
  | ClientCutText
  | QEMUClientMessage
  | Unknown
  | Fail

let string_of_message_type = function
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
  | Unknown ->
      "Unknown"
  | Fail ->
      "Fail"

(* RFB Protocol Parser *)
module RfbParser = struct
  (* Helper function to format binary data as hex string, up to max_bytes *)
  let hex_dump_data data max_bytes =
    let len = min (String.length data) max_bytes in
    let hex_chars = ref [] in
    for i = 0 to len - 1 do
      hex_chars := Printf.sprintf "%02x" (Char.code data.[i]) :: !hex_chars
    done ;
    let hex_str = String.concat " " (List.rev !hex_chars) in
    if String.length data > max_bytes then
      hex_str ^ "..."
    else
      hex_str

  (*
    https://github.com/rfbproto/rfbproto/blob/master/rfbproto.rst#client-to-server-messages
    Currently, only supports RFB version 3.3, and parses the most common used messages.
    Can be extended to support more message types as needed.
  *)

  (* Parse ProtocolVersion: "RFB 003.003\n" (12 bytes) *)
  let parse_protocol_version =
    take 12 >>| fun data ->
    if data = "RFB 003.003\n" then
      true
    else
      false

  (* Parse ClientInit: 1 byte shared-flag (0 or 1) *)
  let parse_client_init =
    take 1 >>| fun data ->
    let shared_flag = Char.code data.[0] in
    if shared_flag = 0 || shared_flag = 1 then
      true
    else
      false

  (* Parse complete handshake using 'both' combinator *)
  let parse_handshake =
    both parse_protocol_version parse_client_init >>| fun (proto_ok, init_ok) ->
    if proto_ok && init_ok then
      Handshake
    else
      Unknown

  (* Parse SetPixelFormat: message-type(1) + padding(3) + pixel-format(16) = 20 bytes *)
  let parse_set_pixel_format =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 0 then
      take 19 >>| fun _ -> SetPixelFormat
    else
      fail "Not SetPixelFormat"

  (* Parse SetEncodings: message-type(1) + padding(1) + num-encodings(2) + encodings(4*n) *)
  let parse_set_encodings =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 2 then
      take 1 >>= fun _ ->
      Angstrom.BE.any_uint16 >>= fun num_encodings ->
      take (num_encodings * 4) >>| fun _ -> SetEncodings
    else
      fail "Not SetEncodings"

  (* Parse FramebufferUpdateRequest: message-type(1) + incremental(1) + x(2) + y(2) + width(2) + height(2) = 10 bytes *)
  let parse_framebuffer_update_request =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 3 then
      take 9 >>| fun _ -> FramebufferUpdateRequest
    else
      fail "Not FramebufferUpdateRequest"

  (* Parse KeyEvent: message-type(1) + down-flag(1) + padding(2) + key(4) = 8 bytes *)
  let parse_key_event =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 4 then
      take 7 >>| fun _ -> KeyEvent
    else
      fail "Not KeyEvent"

  (* Parse PointerEvent: message-type(1) + button-mask(1) + x(2) + y(2) = 6 bytes *)
  let parse_pointer_event =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 5 then
      take 5 >>| fun _ -> PointerEvent
    else
      fail "Not PointerEvent"

  (* Parse ClientCutText: message-type(1) + padding(3) + length(4) + text(length) *)
  let parse_client_cut_text =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 6 then
      take 3 >>= fun _ ->
      Angstrom.BE.any_int32 >>= fun text_length ->
      take (Int32.to_int text_length) >>| fun _ -> ClientCutText
    else
      fail "Not ClientCutText"

  (* Parse QEMU Client Message: message-type(1) + submessage-type(1) + data(10) = 12 bytes *)
  let parse_qemu_client_message =
    take 1 >>= fun msg_type ->
    if Char.code msg_type.[0] = 255 then
      take 11 >>| fun _ -> QEMUClientMessage
    else
      fail "Not QEMUClientMessage"

  (* Fallback parser for unknown messages *)
  let parse_unknown_message = return Unknown

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
        ; parse_unknown_message (* Fallback *)
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
          parser: rfb_message_type Angstrom.Buffered.state
        ; handshake_completed: bool
        ; (* Rolling buffer of last 50 bytes for debugging
             When an error occurs, the last 50 bytes of data are logged for debugging *)
          debug_buffer: string
      }
    end in
    (* Helper to update debug buffer with new data, keeping last 50 bytes *)
    let update_debug_buffer buffer new_data =
      let combined = buffer ^ new_data in
      let len = String.length combined in
      if len <= 50 then
        combined
      else
        String.sub combined (len - 50) 50
    in

    (* Private state hidden in closure *)
    let state =
      ref
        {
          State.parser=
            Angstrom.Buffered.parse (parse_rfb_message false)
        ; handshake_completed= false
        ; debug_buffer= ""
        }
    in

    (* Helper to get unconsumed data as string *)
    let unconsumed_to_string {Angstrom.Buffered.buf; off; len} =
      Bigstringaf.substring buf ~off ~len
    in

    (* Update handshake completion based on parsed message *)
    let update_handshake_state current_completed message_type =
      match message_type with
      | Handshake -> true
      | _ -> current_completed
    in

    let rec process_parser parser messages =
      match parser with
      | Angstrom.Buffered.Done (unconsumed, message_type) ->
          let new_messages = message_type :: messages in
          let has_handshake_completed =
            update_handshake_state !state.State.handshake_completed message_type
          in

          if message_type = Unknown then (
            debug
              "Stopping RFB parsing due to Unknown message. Last 50 bytes: %s"
              (hex_dump_data !state.State.debug_buffer 50) ;
            (* Return Unknown message to caller to stop parsing *)
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
          debug
            "RFB parser failed: %s, unconsumed data (%d bytes), last 50 bytes: \
             %s"
            error_msg unconsumed.len
            (hex_dump_data !state.State.debug_buffer 50) ;
          let fail_messages = Fail :: messages in
          (parser, !state.State.handshake_completed, fail_messages)
    in

    let on_data data_chunk =
      let updated_debug_buffer =
        update_debug_buffer !state.State.debug_buffer data_chunk
      in
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
        ; debug_buffer= updated_debug_buffer
        } ;
      List.rev_map string_of_message_type new_messages
    in

    (* Return the data processing function *)
    on_data
end
