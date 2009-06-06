(* ------------------------------------------------------------------

   Copyright (c) 2006 Xensource Inc

   Contacts: Dave Scott    <david.scott@xensource.com>

 
   HTML backend for datamodel

   ------------------------------------------------------------------- *)

open Printf
open Stringext

open Datamodel_types
open Datamodel
open Datamodel_utils
open Dm_api
open Html_common

let all ?(title="XenEnterprise Management API") dotbase api = 
  diagram_basename := dotbase;
  document_title := title;

  Html_imagemap.go api;
  Html_autogen.go api

