(** This module tests the dmidecode processing
 *)
open Bios_strings

let load_test_data file =
  Xapi_stdext_unix.Unixext.string_of_file @@ "test_data/" ^ file

let baseboard_two_string = load_test_data "bios_baseboard_two.dmidecode"

let baseboard_two_record =
  [
    {
      name= "Base Board Information"
    ; values=
        [
          ("Manufacturer", "TestA Inc.")
        ; ("Product Name", "Not Specified")
        ; ("Version", "A0")
        ; ("Serial Number", " .DEADBEEF")
        ; ("Asset Tag", "          ")
        ; ( "Features"
          , "Board is a hosting board\n\
             Board is removable\n\
             Board is replaceable\n\
             Board is hot swappable" )
        ; ("Location In Chassis", "Slot 00")
        ; ("Type", "Server Blade")
        ]
    }
  ; {
      name= "Base Board Information"
    ; values=
        [
          ("Manufacturer", "TestB Inc.")
        ; ("Product Name", "      ")
        ; ("Version", "   ")
        ; ("Serial Number", "FOOBAR")
        ; ("Asset Tag", "          ")
        ; ( "Features"
          , "Board is removable\nBoard is replaceable\nBoard is hot swappable"
          )
        ; ("Location In Chassis", "Slot 00")
        ; ("Type", "Interconnect Board")
        ]
    }
  ]

let baseboard_two =
  [
    ("baseboard-manufacturer", "TestA Inc.")
  ; ("baseboard-product-name", "")
  ; ("baseboard-version", "A0")
  ; ("baseboard-serial-number", ".DEADBEEF")
  ]

let baseboard_empty =
  [
    ("baseboard-manufacturer", "")
  ; ("baseboard-product-name", "")
  ; ("baseboard-version", "")
  ; ("baseboard-serial-number", "")
  ]

let oem_string = load_test_data "bios_oem.dmidecode"

let oem_empty =
  [
    ("oem-1", "Xen")
  ; ("oem-2", "MS_VM_CERT/SHA1/bdbeb6e0a816d43fa6d3fe8aaef04c2bad9d3e3d")
  ]

let oem =
  oem_empty
  @ [
      ("oem-3", "Test System")
    ; ("oem-4", "1[087C]")
    ; ("oem-5", "3[1.0]")
    ; ("oem-6", "12[www.test.com]")
    ; ("oem-7", "14[1]")
    ; ("oem-8", "15[0]")
    ; ("oem-9", "27[10567471934]")
    ]

let invalid_string = load_test_data "bios_invalid.dmidecode"

let with_array_string = load_test_data "bios_with_array.dmidecode"

let with_array =
  [
    {
      name= "BIOS Language Information"
    ; values=
        [
          ("Installable Languages", "en|US|iso8859-1\n<BAD INDEX>")
        ; ("Currently Installed Language", "en|US|iso8859-1")
        ]
    }
  ]

let pp_record fmt {name; values} =
  Format.fprintf fmt "{name=%s; values=%a}" name
    Fmt.(Dump.list @@ pair ~sep:comma string string)
    values

let alco_record = Alcotest.testable pp_record ( = )

let check_values = Alcotest.(check @@ list @@ pair string string)

let check_records = Alcotest.(check @@ result (list alco_record) string)

let parse_string = Angstrom.parse_string ~consume:Prefix P.records

let values_from_string str =
  match parse_string str with
  | Ok (r :: _) ->
      r.values
  | Ok [] ->
      []
  | Error msg ->
      []

let test_parser () =
  check_records "Empty string produces an empty list" (Ok []) (parse_string "") ;
  check_records "Invalid records must be discarded and stop the parser" (Ok [])
    (parse_string invalid_string) ;
  check_records "Two records with same name is valid input"
    (Ok baseboard_two_record)
    (parse_string baseboard_two_string) ;
  check_records "Arrays must be parsed as multi-line values" (Ok with_array)
    (parse_string with_array_string)

let test_baseboard () =
  check_values "Baseboard values must have empty values when input is empty"
    baseboard_empty
    (get_baseboard_strings @@ fun _ _ -> []) ;
  check_values "Second baseboard values must be discarded" baseboard_two
    (get_baseboard_strings @@ fun _ _ -> values_from_string baseboard_two_string)

let test_oem () =
  check_values "OEM default values must be used when input is empty" oem_empty
    (get_oem_strings @@ fun _ _ -> []) ;
  check_values "OEM default values must be listed first" oem
    (get_oem_strings @@ fun _ _ -> values_from_string oem_string)

let test =
  [
    ("Test baseboard strings", `Quick, test_baseboard)
  ; ("Test oem strings", `Quick, test_oem)
  ; ("Test parser", `Quick, test_parser)
  ]
