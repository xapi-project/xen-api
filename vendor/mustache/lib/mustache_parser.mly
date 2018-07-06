/*{{{ The MIT License (MIT)

   Copyright (c) 2015 Rudi Grinberg

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to
   deal in the Software without restriction, including without limitation the
   rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
   sell copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
   IN THE SOFTWARE. *)
}}}*/
%{
  open Mustache_types
  open Mustache_types.Locs
  let parse_section start_s end_s contents =
    if start_s = end_s
    then { contents; name=start_s }
    else
      let msg =
        Printf.sprintf "Mismatched section %s with %s"
                       (string_of_dotted_name start_s)
                       (string_of_dotted_name end_s)
      in
      raise (Invalid_template msg)

  let with_loc startpos endpos desc =
    let loc =
      { loc_start = startpos;
        loc_end = endpos } in
    { loc; desc }
%}

%token EOF
%token END
%token <string list> ESCAPE_START
%token <string list> UNESCAPE_START_AMPERSAND
%token <string list> SECTION_INVERT_START
%token <string list> SECTION_START
%token <string list> SECTION_END
%token <int * string> PARTIAL_START
%token <string list> UNESCAPE_START
%token <string> COMMENT
%token UNESCAPE_END

%token <string> RAW

%start mustache
%type <Mustache_types.Locs.t> mustache

%%

section:
  | ss = SECTION_INVERT_START END
    e = mustache_expr
    se = SECTION_END END {
    with_loc $symbolstartpos $endpos
      (Inverted_section (parse_section ss se e))
  }
  | ss = SECTION_START END
    e = mustache_expr
    se = SECTION_END END {
    with_loc $symbolstartpos $endpos
      (Section (parse_section ss se e))
  }

mustache_element:
  | elt = UNESCAPE_START UNESCAPE_END { with_loc $symbolstartpos $endpos (Unescaped elt) }
  | elt = UNESCAPE_START_AMPERSAND END { with_loc $symbolstartpos $endpos (Unescaped elt) }
  | elt = ESCAPE_START END { with_loc $symbolstartpos $endpos (Escaped elt) }
  | elt = PARTIAL_START END {
      with_loc $symbolstartpos $endpos
        (Partial { indent = fst elt;
                   name = snd elt;
                   contents = lazy None })
    }
  | s = COMMENT { with_loc $symbolstartpos $endpos (Comment s) }
  | sec = section { sec }
  | s = RAW { with_loc $symbolstartpos $endpos (String s) }

mustache_expr:
  | elts = list(mustache_element) {
    match elts with
    | [] -> with_loc $symbolstartpos $endpos (String "")
    | [x] -> x
    | xs -> with_loc $symbolstartpos $endpos (Concat xs)
  }

mustache:
  | mexpr = mustache_expr EOF { mexpr }

%%
