open Pervasiveext
open Printf
open Stringext
open Str

open Datamodel_types
open Dm_api
module DT = Datamodel_types
module DU = Datamodel_utils

let gen_error file name params =
  fprintf file "xenapi_exceptions[\"%s\"] = \"%s\"\n" name params.err_doc

let gen_exception_translation_database () =
  let file = open_out "xenapi_errors.lua" in
    fprintf file "xenapi_exceptions = {}\n";
    Hashtbl.iter (gen_error file) Datamodel.errors

let _ = gen_exception_translation_database ()

