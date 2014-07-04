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

{
  open Formula
  open Formula_parser
  open Printf

  let check_func s =
    if not (Hashtbl.mem builtins s) then
      ksprintf failwith "Undefined function %s" s;
    s
}

let digit = ['0'-'9']
let letter = ['A'-'Z' 'a'-'z']
let float =
    '.' digit+ ('e' digit+)?
  | digit+ ('.' digit*)? (['e' 'E'] digit+)?
let alnum_ = ['A'-'Z' '_' 'a'-'z']
let name = alnum_ (alnum_ ['0'-'9' 'A'-'Z' '_' 'a'-'z']*)?

rule lex = parse
  | "#" [^ '\n']* | [' ' '\t']+ { lex lexbuf }
  | '\n' { Lexing.new_line lexbuf; lex lexbuf }
  | "," { COMMA }
  | ":" { COLON }
  | "(" { LPAREN } | ")" { RPAREN }
  | (letter as cj) (digit+ as si) { POS (int_of_string si, int_of_letter cj) }
  | name as s { FUNC (check_func s) }
  | float as s { CONST (Float (float_of_string s)) }
  | ('+' | '-') as c { A2 (check_func (String.make 1 c)) }
  | ('*' | '/') as c { A4 (check_func (String.make 1 c)) }
  | '^' { A6 (check_func "^") }
  | '"' { lex_string (Buffer.create 32) lexbuf }
  | eof { EOF }

and lex_string buf = parse
  | '\\' _ { Buffer.add_char buf '\\'; lex_string buf lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; lex_string buf lexbuf }
  | '"' { CONST (Text (Buffer.contents buf)) }

{
  open Lexing

  let parse_string s =
    let lexbuf = from_string s in
    lexbuf.lex_curr_p <- {
      pos_fname = "*string*";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0
    };
    start lex lexbuf
}
