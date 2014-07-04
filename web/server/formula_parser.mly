/* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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
 */

%{
  open Formula
%}

%token COMMA COLON LPAREN RPAREN EOF
%token<int * int> POS
%token<Formula.value> CONST
%token<string> FUNC A2 A4 A6
%left A2
%left A4
%left A6

%type<Formula.expr> start
%start start
%%
start:
    expr EOF { $1 }
  | EOF { Empty }
  ;

expr:
    CONST { Const $1 }
  | POS COLON POS { Range ($1, $3) }
  | POS { Range ($1, $1) }
  | FUNC LPAREN params RPAREN { Call ($1, Concat $3) }
  | A2 expr { Call ($1, $2) }
  | expr A2 expr { Call ($2, Concat [$1; $3]) }
  | expr A4 expr { Call ($2, Concat [$1; $3]) }
  | expr A6 expr { Call ($2, Concat [$1; $3]) }
  | LPAREN expr RPAREN { $2 }
  ;

params:
    /* empty */ { [] }
  | nonempty_params { $1 }
  ;
nonempty_params:
    expr { [$1] }
  | expr COMMA nonempty_params { $1 :: $3 }
  ;
