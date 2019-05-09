(*
Copyright (c) 2016 Pieter Goetschalckx

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

exception XXH_error

module type XXHASH = sig
  type hash
  type state

  val hash : ?seed:hash -> string -> hash
  val reset : ?seed:hash -> state -> unit
  val update : state -> string -> unit
  val digest : state -> hash

  val with_state : ?seed:hash -> (state -> unit) -> hash
end

let check errorcode =
  if errorcode != 0 then raise XXH_error

module Make (Bindings : Xxhash_bindings.BINDINGS) = struct

  type hash = Bindings.hash
  type state = Bindings.state

  let hash ?(seed=Bindings.default_seed) input =
    let length = String.length input |> Unsigned.Size_t.of_int in
    Bindings.internal_of_hash seed
    |> Bindings.hash input length
    |> Bindings.hash_of_internal

  let create () =
    Bindings.create ()

  let reset ?(seed=Bindings.default_seed) state =
    Bindings.internal_of_hash seed
    |> Bindings.reset state
    |> check

  let update state input =
    let length = String.length input |> Unsigned.Size_t.of_int in
    Bindings.update state input length
    |> check

  let digest state =
    Bindings.digest state |> Bindings.hash_of_internal

  let free state =
    Bindings.free state |> check

  let with_state ?(seed=Bindings.default_seed) f =
    let with_state_unsafe state =
      reset ~seed state;
      f state;
      digest state
    in

    let state = create () in
    
    let output =
      Xapi_stdext_pervasives.Pervasiveext.finally
      (fun () -> with_state_unsafe state)
      (fun () -> free state)
    in
    output
    
end

module C = Xxhash_bindings.C (Xxhash_generated)
module XXH32 : (XXHASH with type hash = nativeint) = Make (C.XXH32)
module XXH64 : (XXHASH with type hash = int64) = Make (C.XXH64)
