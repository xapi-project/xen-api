let product_version () = Inventory.lookup ~default:"" "PRODUCT_VERSION"

let product_version_text () =
  Inventory.lookup ~default:"" "PRODUCT_VERSION_TEXT"

let product_version_text_short () =
  Inventory.lookup ~default:"" "PRODUCT_VERSION_TEXT_SHORT"

let platform_name () = Inventory.lookup ~default:"" "PLATFORM_NAME"

let platform_version () = Inventory.lookup ~default:"0.0.0" "PLATFORM_VERSION"

let product_brand () = Inventory.lookup ~default:"" "PRODUCT_BRAND"

let build_number () = Inventory.lookup ~default:"" "BUILD_NUMBER"

let hostname = "localhost"

let date = Xapi_build_info.date

let parse_xapi_version version =
  try Scanf.sscanf version "%d.%d.%s" (fun maj min rest -> (maj, min, rest))
  with _ ->
    failwith
      (Printf.sprintf "Couldn't determine xapi version from string: '%s'"
         version
      )

let version, xapi_version_major, xapi_version_minor, git_id =
  match Build_info.V1.version () with
  | None ->
      ("0.0.dev", 0, 0, "dev")
  | Some v ->
      let str = Build_info.V1.Version.to_string v in
      let version =
        if String.starts_with ~prefix:"v" str then
          String.sub str 1 (String.length str - 1)
        else
          str
      in
      let maj, min, git_id = parse_xapi_version version in
      (version, maj, min, git_id)

let compare_version version_a version_b =
  let maj_a, min_a, _ = parse_xapi_version version_a in
  let maj_b, min_b, _ = parse_xapi_version version_b in
  let ( <?> ) a b = if a = 0 then b else a in
  Int.compare maj_a maj_b <?> Int.compare min_a min_b <?> 0

let xapi_user_agent =
  "xapi/"
  ^ string_of_int xapi_version_major
  ^ "."
  ^ string_of_int xapi_version_minor
