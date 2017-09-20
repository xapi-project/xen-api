module StringDiff =
struct
  type t = string
  let compare = String.compare
  let pp_printer = Format.pp_print_string
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module StringSet = OUnitDiff.SetMake(StringDiff)

module StringList = OUnitDiff.ListSimpleMake(StringDiff)
