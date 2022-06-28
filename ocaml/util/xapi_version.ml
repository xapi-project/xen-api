let product_version () = Inventory.lookup ~default:"" "PRODUCT_VERSION"

let product_version_text () =
  Inventory.lookup ~default:"" "PRODUCT_VERSION_TEXT"

let product_version_text_short () =
  Inventory.lookup ~default:"" "PRODUCT_VERSION_TEXT_SHORT"

let platform_name () = Inventory.lookup ~default:"" "PLATFORM_NAME"

let platform_version () = Inventory.lookup ~default:"0.0.0" "PLATFORM_VERSION"

let product_brand () = Inventory.lookup ~default:"" "PRODUCT_BRAND"

let build_number () = Inventory.lookup ~default:"" "BUILD_NUMBER"

let git_id = ""

let hostname = "localhost"

let date = Xapi_build_info.date

let version =
  match Build_info.V1.version () with
  | None ->
      "0.0.0"
  | Some v ->
      let str = Build_info.V1.Version.to_string v in
      if String.starts_with ~prefix:"v" str then
        String.sub str 1 (String.length str - 1)
      else
        str

let xapi_version_major, xapi_version_minor =
  try Scanf.sscanf version "%d.%d.%s" (fun maj min _rest -> (maj, min))
  with _ ->
    failwith
      (Printf.sprintf
         "Couldn't determine xapi version - got unexpected XAPI_VERSION='%s'"
         version
      )
