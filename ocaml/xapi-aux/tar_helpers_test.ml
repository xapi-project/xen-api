open Alcotest

module Identity = struct
  type 'a t = 'a

  let ( >>= ) x f = f x

  let return x = x
end

module Buffer_writer = struct
  type out_channel = Buffer.t

  type 'a t = 'a

  let really_write buf cs = Buffer.add_string buf (Cstruct.to_string cs)
end

module Header_writer = Tar.HeaderWriter (Identity) (Buffer_writer)

let content = "<value>probe-test</value>"

let pax_extended =
  Tar.Header.Extended.
    {
      access_time= None
    ; charset= None
    ; comment= None
    ; group_id= None
    ; gname= None
    ; header_charset= None
    ; link_path= None
    ; mod_time= Some 1L
    ; path= None
    ; file_size= None
    ; user_id= None
    ; uname= None
    }

(* Returns the header bytes and the content that follows them. *)
let make_entry ~pax =
  let hdr = Tar.Header.make "ova.xml" (Int64.of_int (String.length content)) in
  let hdr =
    if pax then
      {hdr with Tar.Header.extended= Some pax_extended}
    else
      hdr
  in
  let buf = Buffer.create 2048 in
  Header_writer.write hdr buf ;
  (Buffer.contents buf, content)

(* Calls [f] on the read end of a pipe that holds [data] and then EOF. *)
let with_stream data f =
  let rd, wr = Unix.pipe () in
  Fun.protect
    ~finally:(fun () -> Unix.close rd)
    (fun () ->
      Xapi_stdext_unix.Unixext.really_write_string wr data ;
      Unix.close wr ;
      f rd
    )

let read_rest fd = Xapi_stdext_unix.Unixext.string_of_fd fd

let test_header ~pax () =
  let header, body = make_entry ~pax in
  with_stream (header ^ body) @@ fun fd ->
  match Tar_helpers.probe_header fd with
  | Header {hdr; bytes_read} ->
      check string "file name" "ova.xml" hdr.Tar.Header.file_name ;
      check int "bytes_read is the whole header" (String.length header)
        bytes_read ;
      check string "fd is left at the content" body (read_rest fd)
  | Not_tar _ ->
      fail "expected Header"

let test_not_tar ~magic () =
  let data = magic ^ String.make 1000 '\xa5' in
  with_stream data @@ fun fd ->
  match Tar_helpers.probe_header fd with
  | Not_tar first ->
      check string "returns the first block"
        (String.sub data 0 Tar.Header.length)
        (Cstruct.to_string first) ;
      check string "fd is left after the first block"
        (String.sub data Tar.Header.length
           (String.length data - Tar.Header.length)
        )
        (read_rest fd)
  | Header _ ->
      fail "expected Not_tar"

let test_short_stream () =
  with_stream (String.make 100 'x') @@ fun fd ->
  check_raises "fewer than 512 bytes" End_of_file (fun () ->
      ignore (Tar_helpers.probe_header fd)
  )

(* A broken real header after a valid pax record must raise: the parse failed
   after the first block, so the stream is a broken tar stream. *)
let test_broken_pax () =
  let header, body = make_entry ~pax:true in
  let header = Bytes.of_string header in
  let real_header = Bytes.length header - Tar.Header.length in
  Bytes.set header real_header 'X' ;
  with_stream (Bytes.to_string header ^ body) @@ fun fd ->
  match Tar_helpers.probe_header fd with
  | exception _ ->
      ()
  | Header _ ->
      fail "expected an exception, got Header"
  | Not_tar _ ->
      fail "expected an exception, got Not_tar"

let tests =
  [
    test_case "ustar header" `Quick (test_header ~pax:false)
  ; test_case "pax header" `Quick (test_header ~pax:true)
  ; test_case "zstd stream is not tar" `Quick
      (test_not_tar ~magic:"\x28\xb5\x2f\xfd")
  ; test_case "gzip stream is not tar" `Quick (test_not_tar ~magic:"\x1f\x8b")
  ; test_case "short stream raises" `Quick test_short_stream
  ; test_case "broken pax header raises" `Quick test_broken_pax
  ]

let () = run __MODULE__ [(__MODULE__, tests)]
