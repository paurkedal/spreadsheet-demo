(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

{shared{
  open Eliom_content
  open Eliom_pervasives
  open Unprime
  open Unprime_list
  open Unprime_option

  let (>>=) = Lwt.(>>=)
  let (>|=) = Lwt.(>|=)
}}

let value_class = function
  | Formula.Invalid -> ["invalid"]
  | Formula.Text _ -> ["text"]
  | Formula.Float _ -> ["float"]

(** Client view of a spreadsheet, containing signals and server functions. *)
module Csheet = struct

  type cell = {
    expr : string Eliom_react.S.Down.t;
    set_expr : (string, unit) server_function;
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
	let set_expr = server_function Json.t<string> @@ fun s ->
	  let e = Formula_lexer.parse_string s in
	  Lwt.return (cell.Spreadsheet.cell_set_expr e) in
	let value_info v = (Formula.string_of_value v, value_class v) in
	let value =
	  Eliom_react.S.Down.of_react ~scope:`Site
	    (React.S.l1 value_info cell.Spreadsheet.cell_value) in
	{expr; set_expr; value}
end

{client{
  let edit_pos, set_edit_pos = React.S.create (0, 0)
  let show_formulas, set_show_formulas = React.S.create false
  let error, set_error = React.S.create ""
  let set_edit_pos pos = set_error ""; set_edit_pos pos
}}

(** HTML and JS for viewing and editing [csheet]. *)
let render_sheet csheet =
  let n, m = Csheet.dim csheet in

  let mkcell j k =
    let open Csheet in
    let {expr; set_expr; value} = csheet.(j).(k) in
    Html5.C.node {{
      let set_expr s =
	try_lwt %set_expr s
	with Eliom_lib.Exception_on_server s -> set_error s; Lwt.return_unit in
      let value = React.S.l1 fst %value in
      let pick showf pos v e = if showf || pos = (%j, %k) then e else v in
      Html5.F.td
	~a:[Html5.F.a_onclick (fun _ -> set_edit_pos (%j, %k));
	    Html5.R.a_class (React.S.l1 snd %value)]
	[Rform5.string_input ~a:[Html5.F.a_size 12] ~onchange:set_expr
			  (React.S.l4 pick show_formulas edit_pos value %expr)]
    }} in

  let mkrow j =
    Html5.F.(tr (th [pcdata (string_of_int j)] :: List.sample (mkcell j) m)) in

  let mkhdr k =
    Html5.F.(th [pcdata (String.make 1 (Formula.letter_of_int k))]) in

  Html5.F.(div [
    div [
      span ~a:[a_class ["global"]] [
	Html5.C.node {{Rform5.checkbox ~onchange:set_show_formulas ()}};
	pcdata "Show formulas.";
      ];
      span ~a:[a_class ["error"]] [Html5.C.node {{Html5.R.pcdata error}}];
    ];
    table ~a:[a_class ["sheet"]]
      (tr (td [] :: List.sample mkhdr m))
      (List.sample mkrow n)
  ])

let sheet = Spreadsheet.create 24 10
let () =
  let open Formula in
  let open Spreadsheet in
  set sheet 0 0 (Const (Text "x ="));
  set sheet 1 0 (Const (Text "y ="));
  set sheet 2 0 (Const (Text "x + y ="));
  set sheet 0 1 (Const (Float 10.0));
  set sheet 1 1 (Const (Float 11.0));
  set sheet 2 1 (Call ("sum", (Range ((0, 1), (1, 1)))))

let csheet = Csheet.of_sheet sheet

let main_handler () () =
  let open Html5.D in
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"Spreadsheet Demo"
      ~css:[["css"; "spreadsheet-demo.css"]]
      (body [
	h1 [pcdata "Spreadsheet Demo"];
	render_sheet csheet;
      ])

module Main_app =
  Eliom_registration.App (struct let application_name = "main" end)
let main_service =
  Main_app.register_service ~path:[] ~get_params:Eliom_parameter.unit
			    main_handler
