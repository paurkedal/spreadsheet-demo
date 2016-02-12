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

open Formula
open Formula_lexer

type cell = {
  cell_expr : expr React.S.t;
  cell_set_expr : expr -> unit;
  mutable cell_value : value React.S.t;
}

type t = cell array array

let dim sheet =
  let n = Array.length sheet in
  if n = 0 then (0, 0) else
  n, Array.length sheet.(0)

let rec eval_expr sheet = function
  | Empty -> []
  | Const v -> [React.S.const v]
  | Range ((iL, jL), (iU, jU)) ->
    let rec collect_row i j acc =
      if j < jL then acc else
      let v = sheet.(i).(j).cell_value in
      let v' = if React.S.value v = HiZ then React.S.const Invalid else v in
      collect_row i (j - 1) (v' :: acc) in
    let rec collect i acc =
      if i < iL then acc else
      collect (i - 1) (collect_row i jU acc) in
    collect iU []
  | Concat es -> List.concat (List.map (eval_expr sheet) es)
  | Call (f, e) -> [Hashtbl.find Formula.builtins f (eval_expr sheet e)]

let create n m =
  let sheet =
    Array.init n @@ fun _ ->
    Array.init m @@ fun _ ->
    let cell_expr, cell_set_expr = React.S.create Empty in
    {cell_expr; cell_set_expr; cell_value = React.S.const Invalid} in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      let aux e =
        match eval_expr sheet e with
        | [s] -> s
        | [] -> React.S.const (Text "")
        | _ -> React.S.const Invalid in
      sheet.(i).(j).cell_value <- React.S.bind sheet.(i).(j).cell_expr aux
    done
  done;
  sheet

let set sheet i j e =
  sheet.(i).(j).cell_set_expr (Const HiZ);
  sheet.(i).(j).cell_set_expr e
