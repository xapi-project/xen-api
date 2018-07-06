#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "mirage-random" @@ fun c ->
  Ok [ Pkg.lib "pkg/META";
       Pkg.lib ~exts:Exts.interface "src/mirage_random" ;
       Pkg.mllib "std/stdlibrandom.mllib"
     ]
