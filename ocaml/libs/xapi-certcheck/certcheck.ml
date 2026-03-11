(* Pure OCaml certificate checking for UEFI Secure Boot NVRAM variables.
   Uses the pure-OCaml x509 library so that xapi_main does not link against
   libssl / libcrypto. *)

type status =
  | Ok
  | Expired
  | NotYetValid
  | ParseError
  | NoCerts
  | InternalError

type cert_info = {
  subject          : string;
  issuer           : string;
  not_before       : int64;
  not_after        : int64;
  is_expired       : bool;
  is_not_yet_valid : bool;
  variable_name    : string;
}

(* ---- Integer helpers --------------------------------------------------- *)

let get_u32_le b off =
  Char.code (Bytes.get b off)
  lor (Char.code (Bytes.get b (off + 1)) lsl 8)
  lor (Char.code (Bytes.get b (off + 2)) lsl 16)
  lor (Char.code (Bytes.get b (off + 3)) lsl 24)

let get_u64_le b off =
  let lo = get_u32_le b off in
  let hi = get_u32_le b (off + 4) in
  Int64.logor (Int64.of_int lo) (Int64.shift_left (Int64.of_int hi) 32)

(* ---- Well-known GUIDs (16 raw bytes each) ------------------------------ *)

(* {8be4df61-93ca-11d2-aa0d-00e098032b8c} - EFI Global Variable *)
let global_variable_guid =
  "\x61\xdf\xe4\x8b\xca\x93\xd2\x11\xaa\x0d\x00\xe0\x98\x03\x2b\x8c"

(* {d719b2cb-3d3a-4596-a3bc-dad00e67656f} - Image Security Database *)
let image_security_db_guid =
  "\xcb\xb2\x19\xd7\x3a\x3d\x96\x45\xa3\xbc\xda\xd0\x0e\x67\x65\x6f"

(* {a5c059a1-94e4-4aa7-87b5-ab155c2bf072} - EFI_CERT_X509_GUID *)
let cert_x509_guid =
  "\xa1\x59\xc0\xa5\xe4\x94\xa7\x4a\x87\xb5\xab\x15\x5c\x2b\xf0\x72"

let guid_of_bytes b off = Bytes.sub_string b off 16

(* ---- UTF-16LE → UTF-8 -------------------------------------------------- *)

let utf16le_to_utf8 b len =
  let buf = Buffer.create (len / 2) in
  let i = ref 0 in
  ( try
      while !i + 1 < len do
        let ch =
          Char.code (Bytes.get b !i)
          lor (Char.code (Bytes.get b (!i + 1)) lsl 8)
        in
        i := !i + 2 ;
        if ch = 0 then
          raise Exit
        else if ch < 0x80 then
          Buffer.add_char buf (Char.chr ch)
        else if ch < 0x800 then (
          Buffer.add_char buf (Char.chr (0xC0 lor (ch lsr 6))) ;
          Buffer.add_char buf (Char.chr (0x80 lor (ch land 0x3F)))
        ) else (
          Buffer.add_char buf (Char.chr (0xE0 lor (ch lsr 12))) ;
          Buffer.add_char buf (Char.chr (0x80 lor ((ch lsr 6) land 0x3F))) ;
          Buffer.add_char buf (Char.chr (0x80 lor (ch land 0x3F)))
        )
      done
    with Exit -> ()
  ) ;
  Buffer.contents buf

(* ---- NVRAM format constants -------------------------------------------- *)

(* See varstored/certcheck.c and include/serialize.h *)
let nvram_magic = "VARS"

let nvram_max_version = 2

(* Ancillary data block prepended to variable list in format version 2 *)
let ancillary_data_len_v2 = 8 + 0x104

(* Reasonable upper bound on the number of EFI variables *)
let max_variable_count = 10000

(* Maximum name / data lengths from varstored's handler.h *)
let name_limit = 4096

let data_limit = 57344

(* ---- NVRAM variable record --------------------------------------------- *)

type nvram_var = {
    v_name : string  (** UTF-8 decoded variable name *)
  ; v_data : bytes  (** Raw variable data (EFI_SIGNATURE_LIST payload) *)
  ; v_guid : string  (** 16-byte raw GUID *)
}

exception Parse_error

(* Parse the NVRAM variable store blob produced by varstored.
   Format:
     4 bytes  magic "VARS"
     4 bytes  version (uint32 LE, currently 1 or 2)
     8 bytes  variable count (uint64 LE)
     8 bytes  data_len (uint64 LE, unused here)
     [version 2 only: ancillary_data_len_v2 bytes skipped]
     for each variable:
       8 bytes  name_len (uint64 LE)
       name_len bytes  UTF-16LE name
       8 bytes  data_len (uint64 LE)
       data_len bytes  EFI_SIGNATURE_LIST payload
       16 bytes GUID
       4 bytes  attributes (uint32 LE)
       16 bytes EFI_TIME
       32 bytes SHA-256 authenticator cert
*)
let parse_nvram_blob data =
  let len = Bytes.length data in
  if len < 24 then raise Parse_error ;
  if Bytes.sub_string data 0 4 <> nvram_magic then raise Parse_error ;
  let version = get_u32_le data 4 in
  if version > nvram_max_version then raise Parse_error ;
  let count = Int64.to_int (get_u64_le data 8) in
  if count < 0 || count > max_variable_count then raise Parse_error ;
  let pos = ref 24 in
  if version = 2 then (
    if len - !pos < ancillary_data_len_v2 then raise Parse_error ;
    pos := !pos + ancillary_data_len_v2
  ) ;
  let vars = ref [] in
  for _ = 1 to count do
    (* name: 8-byte length prefix + UTF-16LE data *)
    if len - !pos < 8 then raise Parse_error ;
    let name_len = Int64.to_int (get_u64_le data !pos) in
    pos := !pos + 8 ;
    if name_len < 0 || name_len > name_limit then raise Parse_error ;
    if len - !pos < name_len then raise Parse_error ;
    let name_bytes = Bytes.sub data !pos name_len in
    pos := !pos + name_len ;
    let name = utf16le_to_utf8 name_bytes name_len in
    (* data: 8-byte length prefix + payload *)
    if len - !pos < 8 then raise Parse_error ;
    let data_len = Int64.to_int (get_u64_le data !pos) in
    pos := !pos + 8 ;
    if data_len < 0 || data_len > data_limit then raise Parse_error ;
    if len - !pos < data_len then raise Parse_error ;
    let var_data = Bytes.sub data !pos data_len in
    pos := !pos + data_len ;
    (* GUID: 16 bytes *)
    if len - !pos < 16 then raise Parse_error ;
    let guid = guid_of_bytes data !pos in
    pos := !pos + 16 ;
    (* attributes (4) + EFI_TIME (16) + SHA-256 cert (32) = 52 bytes *)
    if len - !pos < 52 then raise Parse_error ;
    pos := !pos + 52 ;
    vars := {v_name= name; v_data= var_data; v_guid= guid} :: !vars
  done ;
  !vars

(* ---- Certificate variable classification ------------------------------- *)

(* Returns Some variable_label if [var] is a Secure Boot certificate
   variable (PK, KEK, db, dbx), or None otherwise. *)
let cert_variable_label var =
  if var.v_guid = global_variable_guid then
    match var.v_name with
    | "PK" | "KEK" ->
        Some var.v_name
    | _ ->
        None
  else if var.v_guid = image_security_db_guid then
    match var.v_name with
    | "db" | "dbx" ->
        Some var.v_name
    | _ ->
        None
  else
    None

(* ---- EFI_SIGNATURE_LIST parsing --------------------------------------- *)

(* EFI_SIGNATURE_LIST header layout (all fields little-endian):
     offset  0: SignatureType    (EFI_GUID, 16 bytes)
     offset 16: SignatureListSize  (uint32)
     offset 20: SignatureHeaderSize (uint32)
     offset 24: SignatureSize    (uint32)
   Total header: 28 bytes.
   Followed by SignatureHeaderSize bytes of opaque header, then a sequence
   of SignatureSize-byte entries each containing a 16-byte owner GUID
   followed by the DER-encoded X.509 certificate. *)
let sig_list_hdr_size = 28

let guid_len = 16

(* Extract DER-encoded X.509 blobs from a raw EFI_SIGNATURE_LIST payload.
   Returns [(der_bytes, variable_label)] for every X.509 entry found. *)
let extract_x509_from_siglist var_data var_label =
  let data = var_data in
  let len = Bytes.length data in
  let pos = ref 0 in
  let certs = ref [] in
  while !pos + sig_list_hdr_size <= len do
    let sig_type = guid_of_bytes data !pos in
    let list_size = get_u32_le data (!pos + 16) in
    let header_size = get_u32_le data (!pos + 20) in
    let sig_size = get_u32_le data (!pos + 24) in
    let list_end = !pos + list_size in
    if list_size < sig_list_hdr_size || list_end > len || sig_size = 0 then
      pos := len (* malformed — stop *)
    else (
      if sig_type = cert_x509_guid && sig_size > guid_len then (
        let sig_start = !pos + sig_list_hdr_size + header_size in
        let sp = ref sig_start in
        while !sp + sig_size <= list_end do
          let cert_off = !sp + guid_len in
          let cert_len = sig_size - guid_len in
          if cert_off + cert_len <= len then
            certs := (Bytes.sub data cert_off cert_len, var_label) :: !certs ;
          sp := !sp + sig_size
        done
      ) ;
      pos := list_end
    )
  done ;
  !certs

(* ---- Ptime helpers ----------------------------------------------------- *)

let ptime_of_unix_seconds secs =
  match Ptime.of_float_s (Int64.to_float secs) with
  | Some t ->
      t
  | None ->
      if secs < 0L then Ptime.min else Ptime.max

let ptime_to_int64 t = Int64.of_float (Ptime.to_float_s t)

let dn_to_string dn = Format.asprintf "%a" X509.Distinguished_name.pp dn

(* ---- Core checking logic ----------------------------------------------- *)

(* Check a single DER-encoded X.509 certificate against [check_ptime].
   Returns (is_expired, is_not_yet_valid, cert_count, cert_info option). *)
let check_one_cert der var_label check_ptime collect_info =
  let cs = Cstruct.of_bytes der in
  match X509.Certificate.decode_der cs with
  | Error _ ->
      (false, false, 0, None)
  | Ok cert ->
      let nb, na = X509.Certificate.validity cert in
      let is_expired = Ptime.is_later check_ptime ~than:na in
      let is_not_yet_valid = Ptime.is_later nb ~than:check_ptime in
      let info =
        if collect_info then
          Some
            {
              subject= dn_to_string (X509.Certificate.subject cert)
            ; issuer= dn_to_string (X509.Certificate.issuer cert)
            ; not_before= ptime_to_int64 nb
            ; not_after= ptime_to_int64 na
            ; is_expired
            ; is_not_yet_valid
            ; variable_name= var_label
            }
        else
          None
      in
      (is_expired, is_not_yet_valid, 1, info)

let check_expiration_impl data check_secs collect_info =
  match (try Some (parse_nvram_blob data) with Parse_error -> None) with
  | None ->
      (ParseError, [])
  | Some vars ->
      let check_ptime = ptime_of_unix_seconds check_secs in
      let total = ref 0 in
      let has_expired = ref false in
      let has_not_yet_valid = ref false in
      let infos = ref [] in
      List.iter
        (fun var ->
          match cert_variable_label var with
          | None ->
              ()
          | Some lbl ->
              List.iter
                (fun (der, vl) ->
                  let exp, nyv, n, info =
                    check_one_cert der vl check_ptime collect_info
                  in
                  total := !total + n ;
                  if exp then has_expired := true ;
                  if nyv then has_not_yet_valid := true ;
                  Option.iter (fun i -> infos := i :: !infos) info
                )
                (extract_x509_from_siglist var.v_data lbl)
        )
        vars ;
      let st =
        if !total = 0 then
          NoCerts
        else if !has_expired then
          Expired
        else if !has_not_yet_valid then
          NotYetValid
        else
          Ok
      in
      (st, !infos)

(* ---- Public API -------------------------------------------------------- *)

let check_expiration data =
  let now = Int64.of_float (Unix.gettimeofday ()) in
  fst (check_expiration_impl data now false)

let check_expiration_at data time =
  fst (check_expiration_impl data time false)

let get_cert_info data =
  let now = Int64.of_float (Unix.gettimeofday ()) in
  check_expiration_impl data now true

let status_string = function
  | Ok ->
      "All certificates are valid"
  | Expired ->
      "At least one certificate is expired"
  | NotYetValid ->
      "At least one certificate is not yet valid"
  | ParseError ->
      "Error parsing NVRAM data"
  | NoCerts ->
      "No certificates found"
  | InternalError ->
      "Internal error"
