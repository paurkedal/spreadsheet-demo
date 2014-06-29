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
  open Unprime_string
  open Printf

  type pos = int * int

  let letter_of_int i =
    if i > 27 then failwith "Column number out of range." else
    Char.chr (i + 0x41)

  let int_of_letter c = Char.(code (uppercase c)) - 0x41

  let string_of_pos (i, j) = sprintf "%c%d" (letter_of_int j) i
  let pos_of_string s =
    (int_of_string (String.slice_from 1 s), int_of_letter s.[0])

  type range = pos * pos

  type value =
    | Invalid
    | Text of string
    | Float of float

  let string_of_value ?(quote = false) = function
    | Invalid -> "invalid"
    | Text s -> if quote then "\"" ^ String.escaped s ^ "\"" else s
    | Float x -> sprintf "%g" x
}}

type func = value React.S.t list -> value React.S.t

and expr =
  | Empty
  | Const of value
  | Range of range
  | Concat of expr list
  | Call of string * expr

let operator_precedence = function
  | "+" | "-" -> 2
  | "*" | "/" -> 4
  | _ -> 10

let rec bprint_expr ?(prec = 0) buf = function
  | Empty -> ()
  | Const v -> Buffer.add_string buf (string_of_value ~quote:true v)
  | Range (p, q) ->
    Buffer.add_string buf (string_of_pos p);
    if p <> q then begin
      Buffer.add_char buf ':';
      Buffer.add_string buf (string_of_pos q)
    end
  | Concat [] -> ()
  | Concat (e :: es) ->
    bprint_expr buf e;
    List.iter (fun e -> Buffer.add_string buf ", "; bprint_expr buf e) es
  | Call (f, e) ->
    let p = operator_precedence f in
    match e with
    | Concat [e0; e1] when p > 0 ->
      if p < prec then Buffer.add_char buf '(';
      bprint_expr ~prec:p buf e0;
      bprintf buf " %s " f;
      bprint_expr ~prec:(p + 1) buf e1;
      if p < prec then Buffer.add_char buf ')'
    | _ ->
      Buffer.add_string buf f;
      if p = 10 then Buffer.add_char buf '(';
      bprint_expr buf e;
      if p = 10 then Buffer.add_char buf ')'

let string_of_expr e =
  let buf = Buffer.create 64 in
  bprint_expr buf e;
  Buffer.contents buf

let builtins = Hashtbl.create 23
let def k f = Hashtbl.add builtins k f

let mF f = function
  | Float x0 -> Float (f x0)
  | _ -> Invalid

let mFF f v0 v1 =
  match v0, v1 with
  | Float x0, Float x1 -> Float (f x0 x1)
  | _, _ -> Invalid

let bFF f = function
  | [s0; s1] -> React.S.l2 (mFF f) s0 s1
  | _ -> React.S.const Invalid

let b_sum = React.S.merge (mFF (+.)) (Float 0.0)
let b_prod = React.S.merge (mFF ( *. )) (Float 1.0)
let b_minus = function
  | [s0] -> React.S.l1 (mF (~-.)) s0
  | [s0; s1] -> React.S.l2 (mFF (-.)) s0 s1
  | _ -> React.S.const Invalid
let b_div = bFF (/.)

let () =
  def "sum" b_sum;
  def "prod" b_prod;
  def "+" b_sum;
  def "-" b_minus;
  def "*" b_prod;
  def "/" b_div
