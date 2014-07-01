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
{client{
  open Formula
  let edit_pos, set_edit_pos = React.S.create (0, 0)
}}

let value_class = function
  | Formula.Invalid -> ["invalid"]
  | Formula.Text _ -> ["text"]
  | Formula.Float _ -> ["float"]

let render_sheet sheet =
  let n, m = Spreadsheet.dim sheet in

  let mkcell j k =
    let value_info v = (Formula.string_of_value v, value_class v) in
    let s = Eliom_react.S.Down.of_react ~scope:`Site
	      (React.S.l1 value_info sheet.(j).(k).Spreadsheet.cell_value) in
    Html5.F.td
      ~a:[Html5.F.a_onclick {{fun _ -> set_edit_pos (%j, %k)}};
	  Html5.C.attr {{Html5.R.a_class (React.S.l1 snd %s)}}]
       [Html5.C.node {{Html5.R.pcdata (React.S.l1 fst %s)}}] in

  let mkrow j =
    Html5.F.(tr (th [pcdata (string_of_int j)] :: List.sample (mkcell j) m)) in

  let mkhdr k =
    Html5.F.(th [pcdata (String.make 1 (Formula.letter_of_int k))]) in

  let get_editable = server_function Json.t<int * int> @@ fun (j, k) ->
    let cell = sheet.(j).(k) in
    let expr =
      Eliom_react.S.Down.of_react ~scope:`Site
	(React.S.l1 Formula.string_of_expr cell.Spreadsheet.cell_expr) in
    let set_expr s =
      let e = Formula_lexer.parse_string s in
      Lwt.return (cell.Spreadsheet.cell_set_expr e) in
    Lwt.return (expr, server_function Json.t<string> set_expr) in

  let editor = {{
    let error, set_error = React.S.create "" in
    let editable, set_editable =
      React.S.create (React.S.const "", (fun _ -> Lwt.return_unit)) in
    let set_editable cell = set_error ""; set_editable cell in
    Lwt.async begin fun () ->
      Lwt_react.S.run_s (React.S.l1 %get_editable edit_pos) >|=
      React.S.trace set_editable *> Lwt_react.S.keep
    end;
    let set_expr s =
      try_lwt snd (React.S.value editable) s
      with Eliom_lib.Exception_on_server s -> set_error s; Lwt.return_unit in
    Html5.F.span [
      Rform5.custom_input ~to_string:string_of_pos ~of_string:pos_of_string
			  ~onchange:(Lwt.wrap1 set_edit_pos) edit_pos;
      Rform5.string_input ~onchange:set_expr
			  (React.S.switch (React.S.l1 fst editable));
      Html5.F.(span ~a:[a_class ["error"]]) [Html5.R.pcdata error];
    ]
  }} in

  Html5.F.(div [
    Html5.C.node editor;
    table ~a:[a_class ["sheet"]]
      (tr (td [] :: List.sample mkhdr m))
      (List.sample mkrow n)
  ])

let sheet = Spreadsheet.create 12 8
let () =
  let open Formula in
  let open Spreadsheet in
  set sheet 0 0 (Const (Text "x ="));
  set sheet 1 0 (Const (Text "y ="));
  set sheet 2 0 (Const (Text "x + y ="));
  set sheet 0 1 (Const (Float 10.0));
  set sheet 1 1 (Const (Float 11.0));
  set sheet 2 1 (Call ("sum", (Range ((0, 1), (1, 1)))))

let main_handler () () =
  let open Html5.D in
  Lwt.return @@
    Eliom_tools.D.html
      ~title:"Spreadsheet Demo"
      ~css:[["css"; "spreadsheet-demo.css"]]
      (body [
	h1 [pcdata "Spreadsheet Demo"];
	render_sheet sheet;
      ])

module Main_app =
  Eliom_registration.App (struct let application_name = "main" end)
let main_service =
  Main_app.register_service ~path:[] ~get_params:Eliom_parameter.unit
			    main_handler
