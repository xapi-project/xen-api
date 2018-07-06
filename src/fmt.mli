(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** {!Format} pretty-printer combinators.

    Consult {{!nameconv}naming conventions} for your pretty-printers.

    {b References}
    {ul
    {- The {!Format} module documentation.}
    {- The required reading {!Format} module
       {{:https://ocaml.org/learn/tutorials/format.html}tutorial}.}}

    {e %%VERSION%% - {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1:formatting Formatting} *)

val pf : Format.formatter ->
  ('a, Format.formatter, unit) Pervasives.format -> 'a
(** [pf] is {!Format.fprintf}. *)

val kpf : (Format.formatter -> 'a) -> Format.formatter ->
  ('b, Format.formatter, unit, 'a) format4 -> 'b
(** [kpf] is {!Format.kfprintf}. *)

val strf : ('a, Format.formatter, unit, string) format4 -> 'a
(** [strf] is {!Format.asprintf}.

    {b Note.} When using [strf] {!utf_8} and {!style_renderer} are
    always respectively set to [true] and [`None]. See also
    {!strf_like}. *)

val kstrf : (string -> 'a) ->
  ('b, Format.formatter, unit, 'a) format4 -> 'b
(** [kstrf] is like {!strf} but continuation based. *)

val strf_like : Format.formatter ->
  ('a, Format.formatter, unit, string) format4 -> 'a
(** [strf_like ppf] is like {!strf} except its {!utf_8} and {!style_renderer}
    settings are those of [ppf]. *)

val with_buffer : ?like:Format.formatter -> Buffer.t -> Format.formatter
(** [with_buffer ~like b] is a formatter whose {!utf_8} and {!style_renderer}
    settings are copied from those of {!like} (if provided). *)

(** {1:fmt Formatting to standard outputs} *)

val stdout : Format.formatter
(** [stdout] is the standard output formatter. *)

val stderr : Format.formatter
(** [stderr] is the standard error formatter. *)

val pr : ('a, Format.formatter, unit) format -> 'a
(** [pr] is [pf stdout]. *)

val epr : ('a, Format.formatter, unit) format -> 'a
(** [epr] is [pf stderr]. *)

(** {1:fmt_exns Formatting exceptions} *)

val failwith : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [failwith] is [kstrf failwith], raises {!Pervasives.Failure} with
    a pretty-printed string argument. *)

val invalid_arg : ('a, Format.formatter, unit, 'b) format4 -> 'a
(** [invalid_arg] is [kstrf invalid_arg], raises
    {!Pervasives.Invalid_argument} with a pretty-printed string argument. *)

(** {1 Formatters} *)

type 'a t = Format.formatter -> 'a -> unit
(** The type for formatters of values of type ['a]. *)

val nop : 'a t
(** [nop] formats nothing. *)

val cut : unit t
(** [cut] is {!Format.pp_print_cut}. *)

val sp : unit t
(** [sp] is {!Format.pp_print_space}. *)

val comma : unit t
(** [comma] is {!Fmt.unit}[ ",@ "]. *)

val const : 'a t -> 'a -> unit t
(** [const pp_v v] always formats [v] using [pp_v]. *)

val unit : (unit, Format.formatter, unit) Pervasives.format -> unit t
(** [unit fmt] formats a unit value with the format [fmt]. *)

val fmt : ('a, Format.formatter, unit) Pervasives.format ->
  Format.formatter -> 'a
(** [fmt fmt ppf] is [pf ppf fmt]. If [fmt] is used with a single
    non-constant formatting directive, generates a value of type
    {!t}. *)

val always : (unit, Format.formatter, unit) Pervasives.format -> 'a t
(** [always fmt ppf v] formats any value with the constant format [fmt]. *)

(** {1:basetypes Base type formatters} *)

val bool : bool t
(** [bool] is {!Format.pp_print_bool}. *)

val int : int t
(** [int] is [pf ppf "%d"]. *)

val nativeint : nativeint t
(** [nativeint ppf] is [pf ppf "%nd"]. *)

val int32 : int32 t
(** [int32 ppf] is [pf ppf "%ld"]. *)

val int64 : int64 t
(** [int64 ppf] is [pf ppf "%Ld"]. *)

val uint : int t
(** [uint ppf] is [pf ppf "%u"]. *)

val unativeint : nativeint t
(** [unativeint ppf] is [pf ppf "%nu"]. *)

val uint32 : int32 t
(** [uint32 ppf] is [pf ppf "%lu"]. *)

val uint64 : int64 t
(** [uint64 ppf] is [pf ppf "%Lu"]. *)

val float : float t
(** [float ppf] is [pf ppf "%g".] *)

val float_dfrac : int -> float t
(** [float_dfrac d] rounds the float to the [d]th {e decimal}
    fractional digit and formats the result with ["%g"]. Ties are
    rounded towards positive infinity. The result is only defined
    for [0 <= d <= 16]. *)

val float_dsig : int -> float t
(** [float_dsig d] rounds the normalized {e decimal} significand
    of the float to the [d]th decimal fractional digit and formats
    the result with ["%g"]. Ties are rounded towards positive
    infinity. The result is NaN on infinities and only defined for
    [0 <= d <= 16].

    {b Warning.} The current implementation overflows on large [d]
    and floats. *)

val char : char t
(** [char] is {!Format.pp_print_char}. *)

val string : string t
(** [string] is {!Format.pp_print_string}. *)

val buffer : Buffer.t t
(** [buffer] formats a {!Buffer.t} value's current contents. *)

val exn : exn t
(** [exn] formats an exception. *)

val exn_backtrace : (exn * Printexc.raw_backtrace) t
(** [exn_backtrace] formats an exception backtrace. *)

(** {1:polytypes Polymorphic type formatters}

    These formatters give full control to the client over the
    formatting process and do not wrap the formatted structures with
    boxes. Use the {!Dump} module to quickly format values for
    inspection.  *)

val pair : ?sep:unit t -> 'a t -> 'b t -> ('a * 'b) t
(** [pair ~sep pp_fst pp_snd] formats a pair. The first and second
    projection are formatted using [pp_fst] and [pp_snd] and are
    separated by [sep] (defaults to {!cut}). *)

val option : ?none:unit t -> 'a t -> 'a option t
(** [option ~none pp_v] formats an optional value. The [Some] case
    uses [pp_v] and [None] uses [none] (defaults to {!nop}). *)

val result : ok:'a t -> error:'b t -> ('a, 'b) Result.result t
(** [result ~ok ~error] formats a result value using [ok] for the [Ok]
    case and [error] for the [Error] case. *)

val list : ?sep:unit t -> 'a t -> 'a list t
(** [list sep pp_v] formats list elements. Each element of the list is
    formatted in order with [pp_v]. Elements are separated by [sep]
    (defaults to {!cut}). If the list is empty, this is {!nop}. *)

val array : ?sep:unit t -> 'a t -> 'a array t
(** [array sep pp_v] formats array elements. Each element of the array
    is formatted in order with [pp_v]. Elements are separated by [sep]
    (defaults to {!cut}). If the array is empty, this is {!nop}. *)

val hashtbl : ?sep:unit t -> ('a * 'b) t -> ('a, 'b) Hashtbl.t t
(** [hashtbl ~sep pp_binding] formats the bindings of a hash
    table. Each binding is formatted with [pp_binding] and bindings
    are separated by [sep] (defaults to {!cut}). If the hash table has
    multiple bindings for a given key, all bindings are formatted,
    with the most recent binding first. If the hash table is empty,
    this is {!nop}. *)

val queue : ?sep:unit t -> 'a t -> 'a Queue.t t
(** [queue ~sep pp_v] formats queue elements. Each element of the
    queue is formatted in least recently added order with
    [pp_v]. Elements are separated by [sep] (defaults to {!cut}). If
    the queue is empty, this is {!nop}. *)

val stack : ?sep:unit t -> 'a t -> 'a Stack.t t
(** [stack ~sep pp_v] formats stack elements. Each element of the
    stack is formatted from top to bottom order with [pp_v].  Elements
    are separated by [sep] (defaults to {!cut}). If the stack is
    empty, this is {!nop}. *)

val iter : ?sep:unit t -> (('a -> unit) -> 'b -> unit) -> 'a t -> 'b t
(** [iter ~sep iter pp_elt] formats the iterations of [iter] over a
    value using [pp_elt]. Iterations are separated by [sep] (defaults to
    {!cut}). *)

val iter_bindings : ?sep:unit t -> (('a -> 'b -> unit) -> 'c -> unit) ->
  ('a * 'b) t -> 'c t
(** [iter_bindings ~sep iter pp_binding] formats the iterations of
    [iter] over a value using [pp_binding]. Iterations are separated
    by [sep] (defaults to {!cut}). *)

val using : ('a -> 'b) -> 'b t -> 'a t
(** [using f pp] maps values using [f] and formats them with [pp]. *)

(** Formatters for inspecting OCaml values.

    Formatters of this module dump OCaml value with little control
    over the representation but with good default box structures and,
    whenever possible, using OCaml syntax. *)
module Dump : sig

  (** {1:base Base types formatters} *)

  val signal : int t
  (** [signal] formats an OCaml {{!Sys.sigabrt}signal number} as a C
      POSIX
      {{:http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/signal.h.html}
      constant} or ["SIG(%d)"] the signal number is unknown. *)

  val uchar : Uchar.t t
  (** [uchar] formats an OCaml {!Uchar.t} value using only US-ASCII
      encoded characters according to the Unicode
      {{:http://www.unicode.org/versions/latest/appA.pdf}notational
      convention} for code points. *)

  (** {1:polytypes Polymorphic type formatters} *)

  val pair : 'a t -> 'b t -> ('a * 'b) t
  (** [pair pp_fst pp_snd] formats an OCaml pair using [pp_fst] and [pp_snd]
      for the first and second projection. *)

  val option : 'a t -> 'a option t
  (** [option pp_v] formats an OCaml option using [pp_v] for the [Some]
      case. No parentheses are added. *)

  val result : ok:'a t -> error:'b t -> ('a, 'b) Result.result t
  (** [result ~ok ~error] formats an OCaml result using [ok] for the [Ok]
      case value and [error] for the [Error] case value. No parentheses
      are added. *)

  val list : 'a t -> 'a list t
  (** [list pp_v] formats an OCaml list using [pp_v] for the list
      elements. *)

  val array : 'a t -> 'a array t
  (** [array pp_v] formats an OCaml array using [pp_v] for the array
      elements. *)

  val hashtbl : 'a t -> 'b t -> ('a, 'b) Hashtbl.t t
  (** [hashtbl pp_k pp_v] formats an unspecified representation of the
      bindings of a hash table using [pp_k] for the keys and [pp_v]
      for the values. If the hash table has multiple bindings for a
      given key, all bindings are formatted, with the most recent
      binding first. *)

  val queue : 'a t -> 'a Queue.t t
  (** [queue pp_v] formats an unspecified representation of an OCaml
      queue using [pp_v] to format its elements, in least recently added
      order. *)

  val stack : 'a t -> 'a Stack.t t
  (** [stack pp_v] formats an unspecified representation of an OCaml
      stack using [pp_v] to format its elements in top to bottom order. *)

  val iter : (('a -> unit) -> 'b -> unit) -> 'b t -> 'a t -> 'b t
  (** [iter iter pp_name pp_elt] formats an unspecified representation
      of the iterations of [iter] over a value using [pp_elt]. The
      iteration is named by [pp_name]. *)

  val iter_bindings : (('a -> 'b -> unit) -> 'c -> unit) -> 'c t -> 'a t
    -> 'b t -> 'c t
  (** [iter_bindings ~sep iter pp_name pp_k pp_v] formats an
      unspecified representation of the iterations of [iter] over a
      value using [pp_k] and [pp_v]. The iteration is named by
      [pp_name]. *) end

(** {1:boxes Boxes} *)

val box : ?indent:int -> 'a t -> 'a t
(** [box ~indent pp ppf] wraps [pp] in a horizontal or vertical box. Break
    hints that lead to a new line add [indent] to the current indentation
    (defaults to [0]). *)

val hbox : 'a t -> 'a t
(** [hbox] is like {!box} but is a horizontal box: the line is not split
    in this box (but may be in sub-boxes). *)

val vbox : ?indent:int -> 'a t -> 'a t
(** [vbox] is like {!box} but is a vertical box: every break hint leads
    to a new line which adds [indent] to the current indentation
    (default to [0]). *)

val hvbox : ?indent:int -> 'a t -> 'a t
(** [hvbox] is like {!box} but is either {!hbox} if its fits on
    a single line or {!vbox} otherwise. *)

(** {1:bracks Brackets} *)

val parens : 'a t -> 'a t
(** [parens pp_v ppf] is [pf "@[<1>(%a)@]" pp_v]. *)

val brackets : 'a t -> 'a t
(** [brackets pp_v ppf] is [pf "@[<1>[%a]@]" pp_v]. *)

val braces : 'a t -> 'a t
(** [braces pp_v ppf] is [pf "@[<1>{%a}@]" pp_v]. *)

val quote : ?mark:string -> 'a t -> 'a t
(** [quote ~mark pp_v ppf] is [pf "@[<1>@<1>%s%a@<1>%s@]" mark pp_v mark],
    [mark] defaults to ["\""], it is always counted as spanning as single
    column (this allows for UTF-8 encoded marks). *)

(** {1:text Words, paragraphs, text and lines}

    {b Note.} These functions only work on US-ASCII strings and/or
    with newlines (['\n']). If you are dealing with UTF-8 strings or
    different kinds of line endings you should use the pretty-printers
    from {!Uuseg_string}.

    {b White space.} White space is one of the following US-ASCII
    characters: space [' '] ([0x20]), tab ['\t'] ([0x09]), newline
    ['\n'] ([0x0A]), vertical tab ([0x0B]), form feed ([0x0C]),
    carriage return ['\r'] ([0x0D]). *)

val words : string t
(** [words] formats words by suppressing initial and trailing
    white space and replacing consecutive white space with
    a single {!Format.pp_print_space}. *)

val paragraphs : string t
(** [paragraphs] formats paragraphs by suppressing initial and trailing
    spaces and newlines, replacing blank lines (a line made only
    of white space) by a two {!Format.pp_force_newline} and remaining
    consecutive white space with a single {!Format.pp_print_space}. *)

val text : string t
(** [text] formats text by respectively replacing spaces and newlines in
    the string with {!Format.pp_print_space} and {!Format.pp_force_newline}. *)

val lines : string t
(** [lines] formats lines by replacing newlines (['\n']) in the string
    with calls to {!Format.pp_force_newline}. *)

val text_loc : ((int * int) * (int * int)) t
(** [text_loc] formats a line-column text range according to
    {{:http://www.gnu.org/prep/standards/standards.html#Errors}
    GNU conventions}. *)

(** {1:combi Appending} *)

val append : 'a t -> 'b t -> ('a * 'b) t
(** [append pp_v0 pp_v1 ppf (v0, v1)] is [pp_v0 ppf v0; pp_v1 ppf v1]. *)

val prefix : unit t -> 'a t -> 'a t
(** [prefix pp_pre pp] prefixes [pp] by [pp_pre]. *)

val suffix : unit t -> 'a t -> 'a t
(** [suffix pp_suf pp] suffixes [pp] by [pp_suf]. *)

(** {1 Byte sizes} *)

val byte_size : int t
(** [byte_size] formats a byte size according to its magnitude
    using {{:http://www.bipm.org/en/publications/si-brochure/chapter3.html}
    SI prefixes} up to peta bytes (10{^15}). *)

val bi_byte_size : int t
(** [bi_byte_size] formats a byte size according to its magnitude
    using {{:https://en.wikipedia.org/wiki/Binary_prefix}binary prefixes}
    up to pebi bytes (2{^15}). *)

(** {1:utf8_cond Conditional UTF-8 formatting}

    {b Note.} Since {!Format} is not UTF-8 aware using UTF-8 output
    may derail the pretty printing process. Use the pretty-printers
    from {!Uuseg_string} if you are serious about UTF-8 formatting. *)

val if_utf_8 : 'a t -> 'a t -> 'a t
(** [if_utf_8 pp_u pp ppf v] is:
    {ul
    {- [pp_u ppf v] if [utf_8 ppf] is [true].}
    {- [pp ppf v] otherwise.}} *)

val utf_8 : Format.formatter -> bool
(** [utf_8 ppf] is [true] if UTF-8 output is enabled on [ppf]. If
    {!set_utf_8} hasn't been called on [ppf] this is [true]. *)

val set_utf_8 : Format.formatter -> bool -> unit
(** [set_utf_8 ppf b] enables or disables conditional UTF-8 formatting
    on [ppf].

    {b Warning.} Using this function replaces any {!Format.tag} functions
    that may be in place.

    @raise Invalid_argument if [ppf] is {!Format.str_formatter}: it is
    is always UTF-8 enabled. *)

(** {1:styled Styled formatting} *)

type style =
  [ `Bold
  | `Underline
  | `Black
  | `Red
  | `Green
  | `Yellow
  | `Blue
  | `Magenta
  | `Cyan
  | `White
  | `None ]
(** The type for styles. *)

val styled : style -> 'a t -> 'a t
(** [styled s pp] formats like [pp] but styled with [s]. *)

val styled_unit : style -> (unit, Format.formatter, unit) Pervasives.format ->
  unit t
(** [styled_unit s fmt] is [style s (unit fmt)]. *)

(** {2 Style rendering control} *)

type style_renderer = [ `Ansi_tty | `None ]
(** The type for style renderers.
    {ul
    {- [`Ansi_tty], renders styles using
       {{:http://www.ecma-international.org/publications/standards/Ecma-048.htm}
       ANSI escape sequences}.}
    {- [`None], styled rendering has no effect.}} *)

val style_renderer : Format.formatter  -> style_renderer
(** [style_renderer ppf] is the style renderer used by [ppf].  If
    {!set_style_renderer} has never been called on [ppf] this is
    [`None]. *)

val set_style_renderer : Format.formatter -> style_renderer -> unit
(** [set_style_renderer ppf r] sets the style renderer of [ppf] to [r].

    {b Warning.} Using this function replaces any {!Format.tag} functions
    that may be in place.

    @raise Invalid_argument if [ppf] is {!Format.str_formatter}: its
    renderer is always [`None]. *)

(** {1:stringconverters Converting with string value converters} *)

val of_to_string : ('a -> string) -> 'a t
(** [of_to_string f ppf v] is [string ppf (f v)]. *)

val to_to_string : 'a t -> 'a -> string
(** [to_to_string pp_v v] is [strf "%a" pp_v v]. *)

(** {1:nameconv Naming conventions}

    Given a type [ty] use:

    {ul
    {- [pp_ty] for a pretty printer that provides full control to the
       client and does not wrap the formatted value in an enclosing
       box. See {{!polytypes}these examples}.}
    {- [dump_ty] for a pretty printer that provides little control
       over the pretty-printing process, wraps the rendering in an
       enclosing box and tries as much as possible to respect the
       OCaml syntax. These pretty-printers should make it easy to
       inspect and understand values of the given type, they are
       mainly used for quick printf debugging and/or toplevel interaction.
       See {{!Dump.polytypes} these examples}.}}

    If you are in a situation where making a difference between [dump_ty]
    and [pp_ty] doesn't make sense then use [pp_ty].

    For a type [ty] that is the main type of the module (the "[M.t]"
    convention) drop the suffix, that is simply use [M.pp] and
    [M.dump]. *)

(*---------------------------------------------------------------------------
   Copyright (c) 2014 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
