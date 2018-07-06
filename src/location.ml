open Import

module L = Ocaml_common.Location

type t = location =
  { loc_start : Lexing.position
  ; loc_end   : Lexing.position
  ; loc_ghost : bool
  }

let in_file name =
  let loc =
    { pos_fname = name
    ; pos_lnum  = 1
    ; pos_bol   = 0
    ; pos_cnum  = -1
    }
  in
  { loc_start = loc
  ; loc_end   = loc
  ; loc_ghost = true
  }

let none = in_file "_none_"

let raise_errorf ?loc fmt = L.raise_errorf ?loc fmt
let report_exception = L.report_exception

let of_lexbuf (lexbuf : Lexing.lexbuf) =
  { loc_start = lexbuf.lex_start_p
  ; loc_end   = lexbuf.lex_curr_p
  ; loc_ghost = false
  }

let print ppf t =
  Caml.Format.fprintf ppf "File \"%s\", line %d, characters %d-%d:"
    t.loc_start.pos_fname
    t.loc_start.pos_lnum
    (t.loc_start.pos_cnum - t.loc_start.pos_bol)
    (t.loc_end.pos_cnum   - t.loc_start.pos_bol)

type nonrec 'a loc = 'a loc =
  { txt : 'a
  ; loc : t
  }

module Error = struct
  type t = L.error

  let createf ~loc fmt = L.errorf ~loc fmt

  let message (t : t) = t.msg
  let set_message (t : t) msg = { t with msg }

  let register_error_of_exn = L.register_error_of_exn

  let of_exn = Compiler_specifics.error_of_exn

  let rec to_extension (t : t) =
    let loc = t.loc in
    let str s =
      { pstr_loc  = loc
      ; pstr_desc =
          Pstr_eval
            ({ pexp_loc = loc
             ; pexp_attributes = []
             ; pexp_desc = Pexp_constant (Pconst_string (s, None))
             }, [])
      }
    in
    ({ loc = t.loc; txt = "ocaml.error" },
     PStr (str t.msg          ::
           str t.if_highlight ::
           List.map t.sub ~f:(fun t ->
             { pstr_loc = loc
             ; pstr_desc = Pstr_extension (to_extension t, [])
             })))
end

exception Error of Error.t

let () =
  Caml.Printexc.register_printer (function
    | Error e -> Some (Error.message e)
    | _ -> None)
