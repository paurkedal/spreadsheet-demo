(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%shared.start]

type pos = int * int

val letter_of_int : int -> char

val int_of_letter : char -> int

type range = pos * pos

type value =
  | HiZ
  | Invalid
  | Text of string
  | Float of float

val string_of_value : ?quote: bool -> value -> string

[%%server.start]

type expr =
  | Empty
  | Const of value
  | Range of range
  | Concat of expr list
  | Call of string * expr

val string_of_expr : expr -> string

val builtins : (string, value React.S.t list -> value React.S.t) Hashtbl.t
