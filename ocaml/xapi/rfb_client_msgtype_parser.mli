(** RFB (Remote Framebuffer) Protocol Parser for VNC Console Connections
    
    This module provides a stateful parser only for RFB client-to-server messages,
    aiming to identify message types from clients.
*)

(** RFB message types that can be parsed from client data *)
type rfb_message_type =
  | ProtocolVersion  (** "RFB xxx.yyy\n" - 12 bytes *)
  | ClientInit  (** Client initialization - 1 byte *)
  | SetPixelFormat  (** Set pixel format - 20 bytes *)
  | SetEncodings  (** Set encodings - variable length *)
  | FramebufferUpdateRequest  (** Request framebuffer update - 10 bytes *)
  | KeyEvent  (** Keyboard input - 8 bytes *)
  | PointerEvent  (** Mouse/pointer input - 6 bytes *)
  | ClientCutText  (** Clipboard text - variable length *)
  | QEMUClientMessage  (** QEMU-specific message - 12 bytes *)
  | Unknown  (** Unrecognized message type *)
  | Fail  (** Parser failure *)

val string_of_message_type : rfb_message_type -> string
(** Convert message type to string representation *)

(** Protocol state tracking *)
type protocol_state =
  | WaitingProtocolVersion  (** Expecting RFB protocol version *)
  | WaitingClientInit  (** Expecting client initialization *)
  | WaitingClientMessages  (** Expecting regular client messages *)

(** RFB Protocol Parser module *)
module RfbParser : sig
  val create : unit -> string -> string list
  (** Create a new RFB parser instance.
      
      Returns a function that processes incoming data chunks and returns
      a list of parsed message type names as strings.
      
      Usage:
      {[
        let parser = RfbParser.create () in
        let messages = parser data_chunk in
        List.iter (fun msg -> 
          match msg with
          | "Unknown" | "Fail" -> handle_error msg
          | _ -> process_message msg
        ) messages
      ]}
      
      The parser maintains internal state including:
      - Current protocol state (version negotiation, init, or messages)
      - Rolling debug buffer (last 50 bytes for error debugging)
      - Parser state for handling partial messages
      
      Error handling:
      - Returns ["Unknown"] when encountering unrecognized message types
      - Returns ["Fail"] when parsing fails due to malformed data
      - Caller should stop using parser after receiving Unknown/Fail messages
      
      @return function of type (string -> string list) that processes data chunks
  *)
end
