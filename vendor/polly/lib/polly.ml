module Events = struct
  type t = int

  external polly_IN : unit -> t = "caml_polly_EPOLLIN"

  external polly_PRI : unit -> t = "caml_polly_EPOLLPRI"

  external polly_OUT : unit -> t = "caml_polly_EPOLLOUT"

  external polly_RDNORM : unit -> t = "caml_polly_EPOLLRDNORM"

  external polly_RDBAND : unit -> t = "caml_polly_EPOLLRDBAND"

  external polly_WRNORM : unit -> t = "caml_polly_EPOLLWRNORM"

  external polly_WRBAND : unit -> t = "caml_polly_EPOLLWRBAND"

  external polly_MSG : unit -> t = "caml_polly_EPOLLMSG"

  external polly_ERR : unit -> t = "caml_polly_EPOLLERR"

  external polly_HUP : unit -> t = "caml_polly_EPOLLHUP"

  external polly_RDHUP : unit -> t = "caml_polly_EPOLLRDHUP"

  external polly_WAKEUP : unit -> t = "caml_polly_EPOLLWAKEUP"

  external polly_ONESHOT : unit -> t = "caml_polly_EPOLLONESHOT"

  external polly_ET : unit -> t = "caml_polly_EPOLLET"

  (* external polly_EXCLUSIVE : unit -> t = "caml_polly_EPOLLEXCLUSIVE" *)

  let inp = polly_IN ()

  let pri = polly_PRI ()

  let out = polly_OUT ()

  let rdnorm = polly_RDNORM ()

  let rdband = polly_RDBAND ()

  let wrnorm = polly_WRNORM ()

  let wrband = polly_WRBAND ()

  let msg = polly_MSG ()

  let err = polly_ERR ()

  let hup = polly_HUP ()

  let rdhup = polly_RDHUP ()

  let wakeup = polly_WAKEUP ()

  let oneshot = polly_ONESHOT ()

  let et = polly_ET ()

  (* let exclusive = polly_EXCLUSIVE () *)

  let empty = 0

  let all =
    [
      (inp, "in")
    ; (pri, "pri")
    ; (out, "out")
    ; (rdnorm, "rdnorm")
    ; (rdband, "rdband")
    ; (wrnorm, "wrnorm")
    ; (wrband, "wrband")
    ; (msg, "msg")
    ; (err, "err")
    ; (hup, "hup")
    ; (rdhup, "rdhup") (*  ; (exclusive, "exclusive") *)
    ; (wakeup, "wakeup")
    ; (oneshot, "oneshot")
    ; (et, "et")
    ]

  let ( lor ) = ( lor )

  let ( land ) = ( land )

  let lnot = lnot

  let to_string t =
    let add result (event, str) =
      if t land event <> empty then str :: result else result
    in
    List.fold_left add [] all |> String.concat " "

  let test x y = x land y <> empty
end

external caml_polly_add : Unix.file_descr -> Unix.file_descr -> Events.t -> unit
  = "caml_polly_add"

external caml_polly_del : Unix.file_descr -> Unix.file_descr -> Events.t -> unit
  = "caml_polly_del"

external caml_polly_mod : Unix.file_descr -> Unix.file_descr -> Events.t -> unit
  = "caml_polly_mod"

external caml_polly_create1 : unit -> Unix.file_descr = "caml_polly_create1"

external caml_polly_wait :
     Unix.file_descr (* epoll fd *)
  -> int (* max number of fds handled *)
  -> int (* timeout in ms *)
  -> (Unix.file_descr -> Unix.file_descr -> Events.t -> unit)
  -> int (* actual number of ready fds; 0 = timeout *) = "caml_polly_wait"

let create = caml_polly_create1

let add = caml_polly_add

let del t fd = caml_polly_del t fd Events.empty

let upd = caml_polly_mod

let wait = caml_polly_wait
