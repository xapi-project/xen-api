#!/usr/bin/env ocaml
#use "topfind"
#require "topkg-jbuilder.auto"

let () =
  Topkg_jbuilder.describe ~name:"mirage-profile" ()
