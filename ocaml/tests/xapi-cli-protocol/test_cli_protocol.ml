open Cli_protocol

let marshal_unmarshal (a : message) =
  let x = marshal_message a in
  let b, (_, offset) = unmarshal_message (x, 0) in
  if a <> b then
    failwith
      (Printf.sprintf "marshal_unmarshal failure: %s <> %s"
         (string_of_message a) (string_of_message b)) ;
  if String.length x <> offset then
    failwith
      (Printf.sprintf
         "Failed to consume all data in marshal_unmarshal %s (length=%d \
          offset=%d)"
         (string_of_message a) (String.length x) offset)

let examples =
  [
    Command (Print "Hello there")
  ; Command (Debug "this is debug output")
  ; Command (Load "ova.xml")
  ; Command (HttpGet ("foo.export", "/import"))
  ; Command (HttpPut ("foo.export", "/export"))
  ; Command Prompt
  ; Command (Exit 5)
  ; Response OK
  ; Response Failed
  ; Response Wait
  ; Blob (Chunk 1024l)
  ; Blob (Chunk 10240l)
  ; Blob (Chunk 102400l)
  ; Blob (Chunk 1024000l)
  ; Blob (Chunk 10240000l)
  ; Blob End
  ; Command (Error ("somecode", ["a"; "b"; "c"]))
  ; Command (Error ("another", []))
  ]

let roundtrip_marshaling_tests =
  let test_case case =
    Alcotest.test_case (string_of_message case) `Quick (fun () ->
        marshal_unmarshal case)
  in
  List.map test_case examples

let () =
  Alcotest.run "CLI Protocol"
    [("roundtrip-marshaling", roundtrip_marshaling_tests)]
