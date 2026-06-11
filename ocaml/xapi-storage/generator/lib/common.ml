(* Common definitions used between the different interfaces *)

open Idl
open Rpc

type exnt = Unimplemented of string [@@deriving rpcty]

exception DataExn of exnt

let error =
  Error.
    {
      def= exnt
    ; raiser= (function e -> DataExn e)
    ; matcher= (function DataExn e -> Some e | _ -> None)
    }

let dbg =
  Param.mk ~name:"dbg"
    ~description:["Debug context from the caller"]
    Types.string

let unit = Param.mk Types.unit

type uri = string [@@deriving rpcty]

type blocklist = {blocksize: int; ranges: (int64 * int64) list}
[@@deriving rpcty]
