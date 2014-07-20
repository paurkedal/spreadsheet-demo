# An FRP Experiment

This code implements a multi-user online spreadsheet.  It is not meant for
production use.  Its purpose is rather to test the scalability of functional
reactive programming in Eliom client-server applications.

## Known Issues and Limitaions

* The cell content is switched from value to formula when a cell is clicked,
  but this does not happen if you focus the cell some other way, like
  pressing tab from the previous cell.  Is there a better way to detect
  focus changes and switch editing of cells accordingly?
* There is only one shared spreadsheet and no way to load or save it.
  Which is okay, as it's only meant for testing.

## Usage

After checking out the repository and building according to standard oasis
practises, you run it on http://localhost:8080/ with

    mkdir -p _var/{lib,log,run}
    ocsigenserver -c ocsigen-dev.conf

You can remove 127.0.0.1 from `<port/>` to test from multiple computers or
devices.  Try to connect multiple client simultaneously when editing the
formulas.

The supported formulas are roughly as follows, where operators have the
usual precedence.

    e ::= (e) | - e | e + e | e - e | e * e | e / e | e mod e | e ^ e
        | pos | f(e) | F(es)

    es ::= e | es, es | pos:pos

    pos ::= [a-z][0-9]+

    f ::= sqrt | exp | log | log10 | ceil | floor | abs
        | cos | sin | tan | acos | asin | atan | cosh | sinh | tanh

    F ::= sum | prod | min | max
