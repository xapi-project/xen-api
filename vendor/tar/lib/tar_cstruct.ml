module Cstruct_io = struct
  (* Input from a single Cstruct.t value *)

  type in_channel = {
    mutable pos : int;
    data : Cstruct.t;
  }

  let make_in_channel data =
    { pos = 0; data }

  let check_available ch len =
    min (Cstruct.len ch.data - ch.pos) len

  let really_input ic buf pos len =
    if check_available ic len <> len then raise End_of_file;
    Cstruct.blit_to_bytes ic.data ic.pos buf pos len;
    ic.pos <- ic.pos + len

  let input ic buf pos len =
    let available = check_available ic len in
    Cstruct.blit_to_bytes ic.data ic.pos buf pos available;
    ic.pos <- ic.pos + available;
    available

  (* Output to a list of Cstruct.t values *)

  type out_channel = {
    mutable data : Cstruct.t list;
  }

  let make_out_channel () = { data = [] }

  let output oc buf pos len =
    let elt = Cstruct.create len in
    Cstruct.blit_from_bytes buf pos elt 0 len;
    oc.data <- elt :: oc.data

  let close_out (_ : out_channel) = ()

  let to_string oc =
    Cstruct.copyv (List.rev oc.data)

  let to_cstruct oc =
    Cstruct.concat (List.rev oc.data)
end

include Cstruct_io

include Tar.Make(Cstruct_io)
