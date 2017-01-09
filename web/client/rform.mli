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

open Eliom_content

val custom_input :
    ?a: Html_types.input_attrib Html.attrib list ->
    of_string: (string -> 'a) -> to_string: ('a -> string) ->
    ?onchange: ('a -> unit Lwt.t) ->
    'a React.S.t -> [> Html_types.input] Html.elt

val string_input :
      ?a: Html_types.input_attrib Html.attrib list ->
      ?onchange: (string -> unit Lwt.t) ->
      string React.S.t -> [> Html_types.input] Html.elt
val int_input :
      ?a: Html_types.input_attrib Html.attrib list ->
      ?to_string: (int -> string) ->
      ?onchange: (int -> unit Lwt.t) ->
      int React.S.t -> [> Html_types.input] Html.elt
val float_input :
      ?a: Html_types.input_attrib Html.attrib list ->
      ?to_string: (float -> string) ->
      ?onchange: (float -> unit Lwt.t) ->
      float React.S.t -> [> Html_types.input] Html.elt

val checkbox :
      ?a: Html_types.input_attrib Html.attrib list ->
      onchange: (bool -> unit) ->
      unit -> [> Html_types.input] Html.elt
