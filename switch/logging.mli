(*
Copyright (c) Citrix Systems Inc.
All rights reserved.

Redistribution and use in source and binary forms, 
with or without modification, are permitted provided 
that the following conditions are met:

*   Redistributions of source code must retain the above 
    copyright notice, this list of conditions and the 
    following disclaimer.
*   Redistributions in binary form must reproduce the above 
    copyright notice, this list of conditions and the 
    following disclaimer in the documentation and/or other 
    materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND 
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, 
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF 
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR 
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR 
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF 
SUCH DAMAGE.
*)

(** Non-blocking logging functions. Note you must start the
    [logging_thread] if you want the output to go somewhere.
    Note the log messages are written to a fixed-length buffer
    which can overflow. If it overflows then log messages are
    dropped and this fact is guaranteed to be logged. *)

val debug: ('a, unit, string, unit) format4 -> 'a
val info:  ('a, unit, string, unit) format4 -> 'a
val warn:  ('a, unit, string, unit) format4 -> 'a
val error: ('a, unit, string, unit) format4 -> 'a

val logging_thread: unit -> unit Lwt.t
(** starts the background logging thread: this reads from a
    fixed-size logging buffer and writes to the physical log.
    If log items are dropped due to the buffer overflowing then
    this fact is guaranteed to be logged. *)
