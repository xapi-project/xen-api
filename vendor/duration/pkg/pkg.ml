#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "duration" @@ fun c ->
  Ok [
    Pkg.mllib "src/duration.mllib";
    Pkg.test "test/tests"
  ]
