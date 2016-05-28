#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let license = Pkg.std_file "COPYING"
let change_log = Pkg.std_file ~install:false "pkg/no_changelog.txt"

let build_cmd c os =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-plugin-tag" % "package(ocamlbuild-eliom-dev)"
        % "-build-dir" % build_dir)

let build = Pkg.build ~cmd:build_cmd ()

let () = Pkg.describe ~build ~license ~change_log "spreadsheet-demo" @@ fun c ->
  Ok [
    Pkg.mllib "web/server/spreadsheet-demo.mllib";
    Pkg.share ~dst:"static/" "web/client/spreadsheet_demo.js";
    Pkg.share ~dst:"static/" "web/static/spreadsheet-demo.css";
  ]
