let product_version () = Inventory.lookup ~default:"" "PRODUCT_VERSION" 
let product_version_text () = Inventory.lookup ~default:"" "PRODUCT_VERSION_TEXT" 
let product_version_text_short () = Inventory.lookup ~default:"" "PRODUCT_VERSION_TEXT_SHORT" 
let platform_name () = Inventory.lookup ~default:"" "PLATFORM_NAME" 
let platform_version () = Inventory.lookup ~default:"0.0.0" "PLATFORM_VERSION" 
let product_brand () = Inventory.lookup ~default:"" "PRODUCT_BRAND" 
let build_number () = Inventory.lookup ~default:"" "BUILD_NUMBER" 

let git_id = ""
let hostname = "localhost"
let date = Build_info.date
let xapi_version_major = 1
let xapi_version_minor = 20
