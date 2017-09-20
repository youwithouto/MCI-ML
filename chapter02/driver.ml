open Core
open Lexer
open Lexing

let rec prt lexbuf =
  let v = Lexer.read lexbuf in
  match v with
  | ("EOF", l, c) ->
    printf "%s\t(%d:%d)\n" "eof" l c;
  | (token, l, c) ->
    printf "%s\t(%d:%d)\n" token l c;
    prt lexbuf

let loop filename () =
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  prt lexbuf;
  In_channel.close inx

let () =
  Command.basic ~summary:"Parse and display JSON"
    Command.Spec.(empty +> anon ("filename" %: file))
    loop
  |> Command.run
