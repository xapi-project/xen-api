(* Common definitions used between the different interfaces *)

open Idl
open Rpc

type exnt =
  | Unimplemented of string
[@@deriving rpcty]
exception DataExn of exnt
let error = Error.{
    def = exnt;
    raiser = (function e -> DataExn e);
    matcher = (function | DataExn e -> Some e | _ -> None)
  }

let dbg = Param.mk ~name:"dbg" ~description:["Debug context from the caller"] Types.string
let unit = Param.mk Types.unit
let task_id = Param.mk ~name:"task_id" Types.string

type uri = string [@@deriving rpcty] [@@doc
  ["A URI representing the means for accessing the volume data. The interpretation ";
   "of the URI is specific to the implementation. Xapi will choose which ";
   "implementation to use based on the URI scheme."]]

type blocklist = {
  blocksize : int
      [@doc ["size of the individual blocks"]];
  ranges : (int64 * int64) list
      [@doc ["list of block ranges, where a range is a (start,length) pair, measured in units of [blocksize]"]];
} [@@deriving rpcty] [@@doc ["List of blocks for copying"]]
