open Import

let default_print_warning loc =
  Location.print_error Format.err_formatter loc;
  Format.fprintf Format.err_formatter "this [if] branch is not delimited\n%!"

let about_ite_branch_ref = ref default_print_warning

let care_about_ite_branch = ref false
let about_ite_branch loc = !about_ite_branch_ref loc
