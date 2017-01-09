#! /usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let licenses = [Pkg.std_file "COPYING"]
let change_logs = [Pkg.std_file ~install:false "pkg/no_changelog.txt"]

let classic_display = Conf.(key "classic-display" bool ~absent:true)

let build_cmd c os targets =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  OS.Cmd.run Cmd.(
    ocamlbuild
      % "-use-ocamlfind"
      % "-plugin-tag" % "package(eliom.ocamlbuild)"
      % "-build-dir" % build_dir
      %% on (Conf.debug c) (of_list ["-tag"; "debug"])
      %% on (Conf.value c classic_display) (v "-classic-display")
      %% of_list targets
  )

let build = Pkg.build ~cmd:build_cmd ()

let () =
  Pkg.describe ~build ~licenses ~change_logs "spreadsheet-demo" @@ fun c ->
  Ok [
    Pkg.mllib "web/server/spreadsheet-demo.mllib";
    Pkg.share ~dst:"static/" "web/client/spreadsheet_demo.js";
    Pkg.share ~dst:"static/" "web/static/spreadsheet-demo.css";
  ]
