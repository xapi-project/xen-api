(*{{{ The MIT License (MIT)

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
   IN THE SOFTWARE. }}}*)

type name = string
type dotted_name = string list

let pp_dotted_name fmt = function
  | [] ->
    Format.fprintf fmt "."
  | n :: ns ->
    Format.fprintf fmt "%s" n;
    List.iter (fun n -> Format.fprintf fmt ".%s" n) ns

let string_of_dotted_name n =
  Format.asprintf "%a" pp_dotted_name n

module Locs = struct
  [@@@warning "-30"]

  type loc =
    { loc_start: Lexing.position;
      loc_end: Lexing.position }

  type desc =
    | String of string
    | Escaped of dotted_name
    | Section of section
    | Unescaped of dotted_name
    | Partial of partial
    | Inverted_section of section
    | Concat of t list
    | Comment of string
  and section =
    { name: dotted_name;
      contents: t }
  and partial =
    { indent: int;
      name: name;
      contents: t option Lazy.t }
  and t =
    { loc : loc;
      desc : desc }
end

module No_locs = struct
  [@@@warning "-30"]

  type t =
    | String of string
    | Escaped of dotted_name
    | Section of section
    | Unescaped of dotted_name
    | Partial of partial
    | Inverted_section of section
    | Concat of t list
    | Comment of string
  and section =
    { name: dotted_name;
      contents: t }
  and partial =
    { indent: int;
      name: name;
      contents: t option Lazy.t }
end

exception Invalid_param of string
exception Invalid_template of string
exception Missing_variable of string
exception Missing_section of string
exception Missing_partial of string
