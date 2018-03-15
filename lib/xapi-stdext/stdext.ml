(* New modules *)
module Base64 = Xapi_stdext_base64.Base64
module Date = Xapi_stdext_date.Date
module Encodings = Xapi_stdext_encodings.Encodings
module Range = Xapi_stdext_range.Range

(* Monadic modules *)
module Monad = Xapi_stdext_monadic.Monad
module Either = Xapi_stdext_monadic.Either (* Should be deprecated and replaced by Result *)
module Opt = Xapi_stdext_monadic.Opt

(* Standard library extensions and additions*)
module Pervasiveext = Xapi_stdext_pervasives.Pervasiveext
module Filenameext = Xapi_stdext_std.Filenameext
module Hashtblext = Xapi_stdext_std.Hashtblext
module Listext = Xapi_stdext_std.Listext
module Xstringext = Xapi_stdext_std.Xstringext

module Threadext = Xapi_stdext_threads.Threadext
module Semaphore = Xapi_stdext_threads.Semaphore

module Unixext = Xapi_stdext_unix.Unixext
module Zerocheck = Xapi_stdext_zerocheck.Zerocheck

(* To depracate asap *)
module Fun = Xapi_stdext_deprecated.Fun
