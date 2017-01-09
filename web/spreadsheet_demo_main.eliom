(* Copyright (C) 2014--2017  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

[%%shared
  open Eliom_content.Html
  open Eliom_client
  open Unprime
  open Unprime_list
  open Unprime_option

  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
]

let value_class = function
  | Formula.HiZ     -> ["invalid"]
  | Formula.Invalid -> ["invalid"]
  | Formula.Text _  -> ["text"]
  | Formula.Float _ -> ["float"]

(** Client view of a spreadsheet, containing signals and server functions. *)
module Csheet = struct

  type cell = {
    expr : string Eliom_react.S.Down.t;
    set_expr : (string, (unit, string) result) server_function;
    value : (string * string list) Eliom_react.S.Down.t;
  }

  let dim csheet =
    let n = Array.length csheet in
    if n = 0 then (0, 0) else (n, Array.length csheet.(0))

  let of_sheet sheet =
    let n, m = Spreadsheet.dim sheet in
    Array.init n @@ fun j ->
      Array.init m @@ fun k ->
        let cell = sheet.(j).(k) in
        let expr =
          Eliom_react.S.Down.of_react ~scope:`Site
            (React.S.l1 Formula.string_of_expr cell.Spreadsheet.cell_expr) in
        let set_expr = server_function [%derive.json: string] @@ fun s ->
          Lwt.return begin
            try
              let e = Formula_lexer.parse_string s in
              Spreadsheet.set sheet j k e; Ok ()
            with
             | Parsing.Parse_error -> Error "Invalid expression."
             | Failure msg -> Error msg
          end in
        let value_info v = (Formula.string_of_value v, value_class v) in
        let value =
          Eliom_react.S.Down.of_react ~scope:`Site
            (React.S.l1 value_info cell.Spreadsheet.cell_value) in
        {expr; set_expr; value}
end

[%%client
  let edit_pos, set_edit_pos = React.S.create (0, 0)
  let show_formulas, set_show_formulas = React.S.create false
  let error, set_error = React.S.create ""
  let set_edit_pos pos = set_error ""; set_edit_pos pos
]

(** HTML and JS for viewing and editing [csheet]. *)
let render_sheet csheet =
  let n, m = Csheet.dim csheet in

  let mkcell j k =
    let open Csheet in
    let {expr; set_expr; value} = csheet.(j).(k) in
    C.node ~init:(D.td []) [%client
      let set_expr s =
        ~%set_expr s >|=
        (function
         | Ok () -> set_error ""
         | Error msg -> set_error msg) in
      let value = React.S.l1 fst ~%value in
      let pick showf pos v e = if showf || pos = (~%j, ~%k) then e else v in
      F.td
        ~a:[F.a_onclick (fun _ -> set_edit_pos (~%j, ~%k));
            R.a_class (React.S.l1 snd ~%value)]
        [Rform.string_input ~a:[F.a_size 12] ~onchange:set_expr
                          (React.S.l4 pick show_formulas edit_pos value ~%expr)]
    ] in

  let mkrow j =
    let tds = List.sample (mkcell j) m in
    F.tr (F.th [F.pcdata (string_of_int j)] :: tds) in

  let mkhdr k =
    F.th [F.pcdata (String.make 1 (Formula.letter_of_int k))] in

  F.div [
    F.div [
      F.span ~a:[F.a_class ["global"]] [
        C.node [%client Rform.checkbox ~onchange:set_show_formulas ()];
        F.pcdata "Show formulas.";
      ];
      F.span ~a:[F.a_class ["error"]] [C.node [%client R.pcdata error]];
    ];
    F.table ~a:[F.a_class ["sheet"]]
      (F.tr (F.td [] :: List.sample mkhdr m) :: List.sample mkrow n)
  ]

let () = Ocsigen_config.set_maxrequestbodysizeinmemory 65536

let sheet = Spreadsheet.create 24 10
let () =
  let open Formula in
  let open Spreadsheet in
  set sheet 0 0 (Const (Text "Values:"));
  set sheet 0 1 (Const (Float 1.33));
  set sheet 1 1 (Const (Float (-1.5)));
  set sheet 2 1 (Const (Float 100.0));
  set sheet 3 1 (Const (Float 0.25));
  let range = Range ((0, 1), (3, 1)) in
  let comp j fn =
    set sheet j 0 (Const (Text (String.capitalize_ascii fn ^ ":")));
    set sheet j 1 (Call (fn, range)) in
  comp 4 "sum";
  comp 5 "prod";
  comp 6 "min";
  comp 7 "max"

let csheet = Csheet.of_sheet sheet

let main_handler () () =
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"Spreadsheet Demo"
      ~css:[["spreadsheet-demo.css"]]
      (F.body [
        F.h1 [F.pcdata "Spreadsheet Demo"];
        render_sheet csheet;
      ])

module Main_app = Eliom_registration.App
  (struct
    let application_name = "spreadsheet_demo"
    let global_data_path = None
  end)

let main_service =
  let gp = Eliom_parameter.unit in
  Eliom_service.(create ~path:(Path []) ~meth:(Get gp) ())

let () = Main_app.register main_service main_handler
