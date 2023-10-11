(*
  Copyright (C) Cloud Software Group

  Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the “Software”), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

(**
  Worker {!mod:Threads} support for {!mod:Bechamel}.  
*)

val operations: Bechamel.Measure.witness
(** [operations] is the number of operations performed, potentially concurrently by [test_concurrently].
  This can be useful for deriving operations/s metrics.
*)

val test_concurrently :
     ?threads:int list
  -> allocate:(unit -> 'a)
  -> free:('a -> unit)
  -> name:string
  -> ('a -> unit) Bechamel.Staged.t
  -> Bechamel.Test.t
(** [test_concurrently ?threads ~allocate ~free ~name run] is a benchmark for [run].
  [n] threads are created.
  [allocate ()] is called to allocate a resource, which is reused across multiple invocations of [run] from the same thread,
  and finally [free resource] is called.
  [threads] specifies the number of threads to test with.
  Only the execution time of [run] (plus thread synchronization overhead) is measured, and not [allocate] and [free].    
*)
