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
   IN THE SOFTWARE.  }}}*)
{
  open Lexing
  open Mustache_parser
  open Mustache_types

  let tok_arg f lexbuf =
    let start_p = lexbuf.Lexing.lex_start_p in
    let x = f lexbuf in
    lexbuf.Lexing.lex_start_p <- start_p;
    x

  let with_space space f =
    tok_arg (fun lexbuf ->
      let () = space lexbuf in
      let x = f lexbuf in
      let () = space lexbuf in
      x
    )

  let split_on_char sep s =
    let open String in
    let r = ref [] in
    let j = ref (length s) in
    for i = length s - 1 downto 0 do
      if unsafe_get s i = sep then begin
        r := sub s (i + 1) (!j - i - 1) :: !r;
        j := i
      end
    done;
    sub s 0 !j :: !r

  let split_ident ident =
    if ident = "." then []
    else split_on_char '.' ident
}

let blank = [' ' '\t']*
let newline = ('\n' | "\r\n")
let raw = [^ '{' '}' '\n']*
let id = ['a'-'z' 'A'-'Z' '_' '/'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '/']*
let ident = ('.' | id ('.' id)*)

rule space = parse
  | blank newline { new_line lexbuf; space lexbuf }
  | blank { () }

and id = parse
  | id { lexeme lexbuf }

and ident = parse
  | ident { lexeme lexbuf }

and comment acc = parse
  | "}}"        { String.concat "" (List.rev acc) }
  | raw newline { new_line lexbuf; comment ((lexeme lexbuf) :: acc) lexbuf }
  | raw         { comment ((lexeme lexbuf) :: acc) lexbuf }
  | ['{' '}']   { comment ((lexeme lexbuf) :: acc) lexbuf }

and mustache = parse
  | "{{{"        { UNESCAPE_START (with_space space ident lexbuf |> split_ident) }
  | "{{&"        { UNESCAPE_START_AMPERSAND (with_space space ident lexbuf |> split_ident) }
  | "{{#"        { SECTION_START (with_space space ident lexbuf |> split_ident) }
  | "{{^"        { SECTION_INVERT_START (with_space space ident lexbuf |> split_ident) }
  | "{{/"        { SECTION_END (with_space space ident lexbuf |> split_ident) }
  | "{{>"        { PARTIAL_START (0, with_space space id lexbuf) }
  | "{{!"        { COMMENT (tok_arg (comment []) lexbuf) }
  | "{{"         { ESCAPE_START (with_space space ident lexbuf |> split_ident) }
  | "}}}"        { UNESCAPE_END }
  | "}}"         { END }
  | raw newline  { new_line lexbuf; RAW (lexeme lexbuf) }
  | raw          { RAW (lexeme lexbuf) }
  | ['{' '}']    { RAW (lexeme lexbuf) }
  | eof          { EOF }

{
   let handle_standalone lexer lexbuf =
     let ends_with_newline s =
       String.length s > 0 &&
       s.[String.length s - 1] = '\n'
     in
     let get_loc () = lexbuf.Lexing.lex_curr_p in
     let get_tok () =
       let loc_start = get_loc () in
       let tok = lexer lexbuf in
       let loc_end = get_loc () in
       (tok, loc_start, loc_end)
     in
     let rec slurp_line () =
       let rec loop acc =
         let tok = get_tok () in
         match tok with
         | EOF, _, _ -> tok :: acc
         | RAW s, _, _ when ends_with_newline s -> tok :: acc
         | _ -> loop (tok :: acc)
       in
       List.rev (loop [])
     in
     let is_blank s =
       let ret = ref true in
       for i = 0 to String.length s - 1 do
         if not (List.mem s.[i] [' '; '\t'; '\r'; '\n']) then
           ret := false
       done;
       !ret
     in
     let rec skip_blanks l =
       let rec loop skipped = function
         | (RAW s, _, _) :: toks when is_blank s ->
           loop (skipped + String.length s) toks
         | toks -> (skipped, toks)
       in
       loop 0 l
     in
     let segment_before tail l =
       let rec loop acc = function
         | [] -> List.rev acc
         | l when l == tail -> List.rev acc
         | y :: ys -> loop (y :: acc) ys
       in
       loop [] l
     in
     let is_standalone toks =
       let (skipped, toks) = skip_blanks toks in
       match toks with
       | (SECTION_START _, _, _) :: (END, _, _) :: toks'
       | (SECTION_INVERT_START _, _, _) :: (END, _, _) :: toks'
       | (SECTION_END _, _, _) :: (END, _, _) :: toks'
       | (PARTIAL_START _, _, _) :: (END, _, _) :: toks'
       | (COMMENT _, _, _) :: toks' ->
         let (_, toks_rest) = skip_blanks toks' in
         begin match toks_rest with
         | [] | [(EOF, _, _)] ->
           let toks_standalone =
             segment_before toks' toks |>
             function
             | [(PARTIAL_START (_, p), loc1, loc2); tok_end] ->
               [(PARTIAL_START (skipped, p), loc1, loc2); tok_end]
             | toks -> toks
           in
           Some (toks_standalone, toks_rest)
         | _ -> None
         end
       | _ -> None
     in

     let buffer = ref [] in
     fun () ->
       match !buffer with
       | tok :: toks ->
         buffer := toks; tok
       | [] ->
         let toks = slurp_line () in
         match is_standalone toks with
         | Some (toks_standalone, toks_rest) ->
           buffer := List.tl toks_standalone @ toks_rest;
           List.hd toks_standalone
         | None ->
           buffer := List.tl toks; List.hd toks
}
