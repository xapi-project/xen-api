(** RFB (Remote Framebuffer) Protocol Parser for VNC Console Connections
    
    This module provides a stateful parser only for RFB client-to-server messages,
    aiming to identify message types from clients.
*)

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
          | "BadHandshake" | "UnknownMsg" | "Fail" -> handle_error msg
          | _ -> process_message msg
        ) messages
      ]}
      
      The parser maintains internal state including:
      - Handshake completion status (boolean)
      - Parser state for handling partial messages
      
      Supported message types:
      - "Handshake" - Successful RFB handshake (ProtocolVersion + ClientInit)
      - "SetPixelFormat" - Set pixel format (20 bytes)
      - "SetEncodings" - Set encodings (variable length)
      - "FramebufferUpdateRequest" - Request framebuffer update (10 bytes)
      - "KeyEvent" - Keyboard input (8 bytes)
      - "PointerEvent" - Mouse/pointer input (6 bytes)
      - "ClientCutText" - Clipboard text (variable length)
      - "QEMUClientMessage" - QEMU-specific message (12 bytes)
      
      Error handling:
      - Returns ["BadHandshake"] for failed handshake
      - Returns ["UnknownMsg"] when encountering unrecognized message types
      - Returns ["Fail"] when parsing fails due to malformed data
      - Caller should stop using parser after receiving error messages
      
      @return function of type (string -> string list) that processes data chunks
  *)
end
