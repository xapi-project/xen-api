module Events = struct
  type t = int

  external epoll_IN : unit -> t = "caml_epoll_EPOLLIN"

  external epoll_PRI : unit -> t = "caml_epoll_EPOLLPRI"

  external epoll_OUT : unit -> t = "caml_epoll_EPOLLOUT"

  external epoll_RDNORM : unit -> t = "caml_epoll_EPOLLRDNORM"

  external epoll_RDBAND : unit -> t = "caml_epoll_EPOLLRDBAND"

  external epoll_WRNORM : unit -> t = "caml_epoll_EPOLLWRNORM"

  external epoll_WRBAND : unit -> t = "caml_epoll_EPOLLWRBAND"

  external epoll_MSG : unit -> t = "caml_epoll_EPOLLMSG"

  external epoll_ERR : unit -> t = "caml_epoll_EPOLLERR"

  external epoll_HUP : unit -> t = "caml_epoll_EPOLLHUP"

  external epoll_RDHUP : unit -> t = "caml_epoll_EPOLLRDHUP"

  external epoll_WAKEUP : unit -> t = "caml_epoll_EPOLLWAKEUP"

  external epoll_ONESHOT : unit -> t = "caml_epoll_EPOLLONESHOT"

  external epoll_ET : unit -> t = "caml_epoll_EPOLLET"

  (* external epoll_EXCLUSIVE : unit -> t = "caml_epoll_EPOLLEXCLUSIVE" *)

  let inp = epoll_IN ()

  let pri = epoll_PRI ()

  let out = epoll_OUT ()

  let rdnorm = epoll_RDNORM ()

  let rdband = epoll_RDBAND ()

  let wrnorm = epoll_WRNORM ()

  let wrband = epoll_WRBAND ()

  let msg = epoll_MSG ()

  let err = epoll_ERR ()

  let hup = epoll_HUP ()

  let rdhup = epoll_RDHUP ()

  let wakeup = epoll_WAKEUP ()

  let oneshot = epoll_ONESHOT ()

  let et = epoll_ET ()

  (* let exclusive = epoll_EXCLUSIVE () *)

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

external caml_epoll_add : Unix.file_descr -> Unix.file_descr -> Events.t -> unit
  = "caml_epoll_add"

external caml_epoll_del : Unix.file_descr -> Unix.file_descr -> Events.t -> unit
  = "caml_epoll_del"

external caml_epoll_mod : Unix.file_descr -> Unix.file_descr -> Events.t -> unit
  = "caml_epoll_mod"

external caml_epoll_create1 : unit -> Unix.file_descr = "caml_epoll_create1"

external caml_epoll_wait :
     Unix.file_descr (* epoll fd *)
  -> int (* max number of fds handled *)
  -> int (* timeout in ms *)
  -> (Unix.file_descr -> Unix.file_descr -> Events.t -> unit)
  -> int (* actual number of ready fds; 0 = timeout *) = "caml_epoll_wait"

let create = caml_epoll_create1

let add = caml_epoll_add

let del t fd = caml_epoll_del t fd Events.empty

let upd = caml_epoll_mod

let wait = caml_epoll_wait
