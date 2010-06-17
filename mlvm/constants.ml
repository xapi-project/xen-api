let label_id = "LABELONE"
let sector_size = 512
let sector_sizeL = 512L
let label_size = sector_size
let label_scan_sectors = 4
let label_scan_size = label_scan_sectors * sector_size

let label_type = "LVM2 001"

let extent_size = Int64.mul 4096L 1024L
let extent_size_minus_one = Int64.sub extent_size 1L
let extent_size_in_sectors = Int64.div extent_size sector_sizeL

let fmtt_magic = Stringext.String.implode (List.map Char.chr [0o040;0o114;0o126;0o115;0o062;0o040;0o170;0o133;0o065;0o101;0o045;0o162;0o060;0o116;0o052;0o076])

let mdah_start = 4096L
let mdah_size = Int64.mul 10240L 1024L 

let pe_align = 65536L

let redo_log_lv_name = "mlvm_redo_log"

let mib = Int64.mul 1024L 1024L
let tib = Int64.mul mib mib

(* Ahem, mutable constants? *)
let dummy_mode = ref false 
let dummy_base = ref "/tmp"
let mapper_name = ref "mapper"
let full_provision = ref false
