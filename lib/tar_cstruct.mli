(** {1 Processing tar content with cstruct buffers} *)

type in_channel
type out_channel

val make_in_channel : Cstruct.t -> in_channel
(** [make_in_channel buf] uses [buf] as a source of raw tar content. *)

val make_out_channel : unit -> out_channel
(** [make_out_channel ()] returns a buffer to hold serialized tar content. *)

val to_string : out_channel -> string
(** [to_string oc] returns the contents of [oc] as a string of bytes. *)

val to_cstruct : out_channel -> Cstruct.t
(** [to_cstruct oc] returns the contents of [oc] as a {!Cstruct.t}. *)

val really_read : in_channel -> Cstruct.t -> unit
(** [really_read ic buf] fills [buf] with data from [ic] or raises
    {!Pervasives.End_of_file *)

val really_write : out_channel -> Cstruct.t -> unit
(** [really_write oc buf] writes the full contents of [buf] to [oc]
    or raises End_of_file *)

module Header : sig
  include module type of Tar.Header

  val get_next_header : ?level:compatibility -> in_channel -> t
  (** [get_next_header ?level ic] returns the next header block or fails with
      `Eof if two consecutive zero-filled blocks are discovered. Assumes [ic]
      is positioned at the possible start of a header block. End_of_file is
      thrown if the stream unexpectedly fails *)
end

module Archive : sig
  val with_next_file : in_channel -> (in_channel -> Header.t -> 'a) -> 'a
  (** [with_next_file ic f] Read the next header, apply the function [f] to
      [ic] and the header.  The function should leave [ic] positioned
      immediately after the datablock.  {!really_read} can be used for this
      purpose. Finally the function skips past the zero padding to the next
      header *)

  val list : ?level:Header.compatibility -> in_channel -> Header.t list
  (** List the contents of a tar *)
end
