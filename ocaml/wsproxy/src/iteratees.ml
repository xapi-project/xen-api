(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

open Helpers

type err = string

type stream = Eof of err option | Chunk of string

let string_of_stream = function
  | Eof (Some x) ->
      Printf.sprintf "Eof (Some '%s')" x
  | Eof None ->
      "Eof None"
  | Chunk s ->
      Printf.sprintf "Chunk '%s'" s

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Iteratee (IO : Monad) = struct
  type 'a t =
    | IE_done of 'a
    | IE_cont of err option * (stream -> ('a t * stream) IO.t)

  module IO_Ops = struct
    let ( let* ) = IO.bind

    let return = IO.return
  end

  let return x = IE_done x

  let rec bind i f =
    let open IO_Ops in
    match i with
    | IE_done result ->
        f result
    | IE_cont (e, k) ->
        let docase = function
          | IE_done x, stream -> (
            match f x with
            | IE_cont (None, k) ->
                k stream
            | x ->
                return (x, stream)
          )
          | x, stream ->
              return (bind x f, stream)
        in
        let go s =
          let* p = k s in
          docase p
        in
        IE_cont (e, go)

  let ( >>= ) = bind

  let ie_contM k x = IO.return (IE_cont (None, k), x)

  let ie_doneM res x = IO.return (IE_done res, x)

  let ie_errM msg k x = IO.return (IE_cont (Some msg, k), x)

  let state = function
    | IE_done _ ->
        "Done"
    | IE_cont (None, _) ->
        "Ready"
    | IE_cont (Some e, _) ->
        Printf.sprintf "Error (%s)" e

  (* Simplest iteratees *)

  let rec peek =
    let step st =
      match st with
      | Chunk s ->
          if String.length s = 0 then
            IO.return (peek, st)
          else
            IO.return (IE_done (Some s.[0]), st)
      | _ ->
          IO.return (IE_done None, st)
    in
    IE_cont (None, step)

  let rec head =
    let rec step st =
      match st with
      | Chunk s ->
          if String.length s = 0 then
            IO.return (head, st)
          else
            IO.return
              ( IE_done (Some s.[0])
              , Chunk (String.sub s 1 (String.length s - 1))
              )
      | _ ->
          IO.return (IE_cont (Some "Eof", step), st)
    in
    IE_cont (None, step)

  let writer really_write _ =
    let open IO_Ops in
    let rec step st =
      match st with
      | Chunk s ->
          let* () = really_write s in
          return (IE_cont (None, step), Chunk "")
      | Eof _ ->
          return (IE_done (), st)
    in
    IE_cont (None, step)

  (* More complex one *)

  let break pred =
    let rec step before st =
      match st with
      | Chunk "" ->
          ie_contM (step before) st
      | Chunk s -> (
        match break pred s with
        | _, "" ->
            ie_contM (step (before ^ s)) (Chunk "")
        | str, tail ->
            ie_doneM (before ^ str) (Chunk tail)
      )
      | _ ->
          IO.return (IE_done before, st)
    in
    IE_cont (None, step "")

  let heads str =
    let rec step cnt str stream =
      match (stream, str) with
      | _, "" | Eof _, _ ->
          IO.return (IE_done cnt, stream)
      | Chunk s, str ->
          if String.length s = 0 then
            IO.return (IE_cont (None, step 0 str), stream)
          else if s.[0] = str.[0] then
            let _, tl = split str 1 in
            step (cnt + 1) tl (Chunk (snd (split s 1)))
          else
            IO.return (IE_done cnt, stream)
    in
    IE_cont (None, step 0 str)

  let drop = function
    | 0 ->
        IE_done ()
    | n ->
        let rec step n st =
          match st with
          | Chunk s ->
              let len = String.length s in
              if len < n then
                ie_contM (step (n - len)) (Chunk "")
              else
                ie_doneM () (Chunk (String.sub s n (len - n)))
          | Eof _ ->
              ie_doneM () st
        in
        IE_cont (None, step n)

  let readn = function
    | 0 ->
        IE_done ""
    | n ->
        let rec step acc n st =
          match st with
          | Chunk s ->
              let len = String.length s in
              if len < n then
                ie_contM (step (acc ^ s) (n - len)) (Chunk "")
              else
                let s1, s2 = split s n in
                ie_doneM (acc ^ s1) (Chunk s2)
          | Eof _ ->
              ie_errM "EOF" (step acc n) st
        in
        IE_cont (None, step "" n)

  let read_int8 = readn 1 >>= fun s -> return (unmarshal_int8 s)

  let read_int16 = readn 2 >>= fun s -> return (unmarshal_int16 s)

  let read_int32 = readn 4 >>= fun s -> return (unmarshal_int32 s)

  let drop_while pred =
    let rec step st =
      match st with
      | Chunk s ->
          let news = str_drop_while pred s in
          if news = "" then
            ie_contM step (Chunk "")
          else
            ie_doneM () (Chunk news)
      | Eof _ ->
          ie_doneM () st
    in
    IE_cont (None, step)

  let accumulate =
    let rec step acc st =
      match st with
      | Chunk s ->
          ie_contM (step (acc ^ s)) (Chunk "")
      | Eof _ ->
          ie_doneM acc st
    in
    IE_cont (None, step "")

  let apply f =
    let rec step st =
      match st with
      | Chunk s ->
          f s ; ie_contM step (Chunk "")
      | Eof _ ->
          ie_doneM () st
    in
    IE_cont (None, step)

  let liftI m =
    let step st i =
      match i with
      | IE_cont (None, k) ->
          k st
      | IE_cont (Some _, _) | IE_done _ ->
          IO.return (i, st)
    in
    IE_cont (None, fun s -> IO.bind m (step s))

  (* ****************************** ENUMERATORS *********************************)

  type 'a enumerator = 'a t -> 'a t IO.t

  (* Simplest enumarator *)

  let enum_eof i =
    let open IO_Ops in
    let result =
      match i with
      | IE_cont (None, f) ->
          let* i, _ = f (Eof None) in
          return i
      | _ ->
          return i
    in
    let* it = result in
    match it with
    | IE_done _ | IE_cont (Some _, _) ->
        result
    | _ ->
        failwith "Divergent iteratee"

  let enum_1chunk str =
    let open IO_Ops in
    function
    | IE_cont (None, f) ->
        let* i, _ = f (Chunk str) in
        return i
    | x ->
        return x

  let rec enum_nchunk str n =
    let open IO_Ops in
    match str with
    | "" ->
        return
    | _ -> (
        function
        | IE_cont (None, f) ->
            let s1, s2 = split str n in
            let* i =
              let* i, _ = f (Chunk s1) in
              return i
            in
            enum_nchunk s2 n i
        | x ->
            return x
      )

  let extract_result_from_iteratee = function
    | IE_done x ->
        x
    | _ ->
        failwith "Not done!"

  type 'a enumeratee = 'a t -> 'a t t

  let rec take =
    let step n k s =
      let open IO_Ops in
      match s with
      | Chunk str ->
          let len = String.length str in
          if len < n then
            let* i, _ = k s in
            return (take (n - len) i, Chunk "")
          else
            let s1, s2 = split str n in
            let* i, _ = k (Chunk s1) in
            return (IE_done i, Chunk s2)
      | Eof _ ->
          let* i, _ = k s in
          return (IE_done i, s)
    in
    function
    | 0 ->
        return
    | n -> (
        function
        | IE_cont (None, k) ->
            IE_cont (None, step n k)
        | (IE_cont (Some _, _) | IE_done _) as it ->
            bind (drop n) (fun () -> return it)
      )

  let stream_printer name =
    let rec step k s =
      let open IO_Ops in
      Printf.printf "%s: %s\n" name (string_of_stream s) ;
      let* i, s = k s in
      match i with
      | IE_cont (err, f) ->
          return (IE_cont (err, step f), s)
      | _ ->
          return (IE_done i, s)
    in
    function
    | IE_cont (None, k) ->
        IE_cont (None, step k)
    | (IE_cont (Some _, _) | IE_done _) as it ->
        return it

  let modify f =
    let rec step k s =
      let open IO_Ops in
      match s with
      | Chunk c -> (
          let s =
            try f c
            with e ->
              Printf.printf "got exception %s\n%!" (Printexc.to_string e) ;
              raise e
          in
          let* i, s = k (Chunk s) in
          match i with
          | IE_cont (err, f) ->
              return (IE_cont (err, step f), s)
          | _ ->
              return (IE_done i, s)
        )
      | Eof _ ->
          let* i, _ = k s in
          return (IE_done i, s)
    in
    function
    | IE_cont (None, k) ->
        IE_cont (None, step k)
    | (IE_cont (Some _, _) | IE_done _) as it ->
        return it

  type 'a either = Left of 'a | Right of 'a

  let read_lines =
    let ( >>= ) = bind in
    let iscrlf = function '\r' | '\n' -> true | _ -> false in
    let terminators =
      heads "\r\n" >>= function 0 -> heads "\n" | n -> return n
    in
    let rec lines' acc = break iscrlf >>= fun l -> terminators >>= check acc l
    and check acc l n =
      match (l, n) with
      | _, 0 ->
          return (Left (List.rev acc))
      | "", _ ->
          return (Right (List.rev acc))
      | l, _ ->
          lines' (l :: acc)
    in
    lines' []
end
