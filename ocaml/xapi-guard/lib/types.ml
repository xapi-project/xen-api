module Service = struct
  type t = Varstored | Swtpm [@@deriving rpcty]

  let to_string = function Varstored -> "Varstored" | Swtpm -> "Swtpm"
end

module Tpm = struct
  (* The TPM has 3 kinds of states *)
  type t = {
      permall: string  (** permanent storage *)
    ; savestate: string  (** for ACPI S3 *)
    ; volatilestate: string  (** for snapshot/migration/etc. *)
  }

  type key = Perm | Save | Volatile

  let key_of_swtpm = function
    | "/tpm2-00.permall" ->
        Perm
    | "/tpm2-00.savestate" ->
        Save
    | "/tpm2-00.volatilestate" ->
        Volatile
    | s ->
        Fmt.invalid_arg "Unknown TPM state key: %s" s

  let serialize_key = function Perm -> 0 | Save -> 1 | Volatile -> 2

  let deserialize_key = function
    | 0 ->
        Ok Perm
    | 1 ->
        Ok Save
    | 2 ->
        Ok Volatile
    | s ->
        Error Printf.(sprintf "Unknown TPM state key: %i" s)

  let empty_state = ""

  let empty = {permall= ""; savestate= ""; volatilestate= ""}

  let split_char = ' '

  let join_string = String.make 1 split_char

  let deserialize t =
    match String.split_on_char split_char t with
    | [permall] ->
        (* backwards compat with reading tpm2-00.permall *)
        {permall; savestate= ""; volatilestate= ""}
    | [permall; savestate; volatilestate] ->
        {permall; savestate; volatilestate}
    | splits ->
        Fmt.failwith "Invalid state: too many splits %d" (List.length splits)

  let serialize t =
    (* it is assumed that swtpm has already base64 encoded this *)
    String.concat join_string [t.permall; t.savestate; t.volatilestate]

  let lookup ~key t =
    match key with
    | Perm ->
        t.permall
    | Save ->
        t.savestate
    | Volatile ->
        t.volatilestate

  let update key state t =
    if String.contains state split_char then
      Fmt.invalid_arg
        "State to be stored (%d bytes) contained forbidden separator: %c"
        (String.length state) split_char ;
    match key with
    | Perm ->
        {t with permall= state}
    | Save ->
        {t with savestate= state}
    | Volatile ->
        {t with volatilestate= state}
end
