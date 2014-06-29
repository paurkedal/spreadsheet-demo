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

open Eliom_content

let custom_input ~of_string ~to_string
		 ?(onchange : ('a -> unit Lwt.t) option) v =
  let open Html5.R.Raw in
  let attrs =
    match onchange with
    | None -> [Html5.D.a_readonly `ReadOnly]
    | Some f ->
      let g ev =
	let elt = (Js.Unsafe.coerce (Dom.eventTarget ev)
			  :> Dom_html.inputElement Js.t) in
	Lwt.async @@ fun () -> f (of_string (Js.to_string elt##value)) in
      [a_onchange Eliom_content.Xml.(Caml (CE_client_closure g))] in
  input ~a:(a_value (React.S.map to_string v) :: attrs) ()

let string_input =
  custom_input ~of_string:(fun x -> x) ~to_string:(fun x -> x)
let int_input ?(to_string = string_of_int) =
  custom_input ~of_string:int_of_string ~to_string
let float_input ?(to_string = string_of_float) =
  custom_input ~of_string:float_of_string ~to_string
