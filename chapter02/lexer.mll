{
  open Lexing

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
    {
      pos with pos_bol = lexbuf.lex_curr_pos;
      pos_lnum = pos.pos_lnum + 1
    }

  let l_c lexbuf =
    let pos = Lexing.lexeme_start_p lexbuf in
      (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)

  let tokenise_l_c lexbuf =
    let token = Lexing.lexeme lexbuf in
    let pos = Lexing.lexeme_start_p lexbuf in
      (token, pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1)
}

let integer = ['0'-'9'] | ['1'-'9']['0'-'9']*
(* string, comment *)
let white = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* '\\' '/' '\\' '*'
   '\\' '*' '\\' '/' *)

rule read =
  parse
  | white       { read lexbuf }                                   (* skip while spaces*)
  | newline     { next_line lexbuf; read lexbuf }                 (* skip newlines *)
  | integer     { tokenise_l_c lexbuf }
  | '"'
    { let lc = l_c lexbuf in
      read_string (Buffer.create 17) lc lexbuf }
  | "/*"
    { let bf = Buffer.create 17 in
      let lc = l_c lexbuf in
      Buffer.add_string bf "/*";
      read_comment bf 1 lc lexbuf }
  | "while"     { tokenise_l_c lexbuf }
  | "for"       { tokenise_l_c lexbuf }
  | "to"        { tokenise_l_c lexbuf }
  | "break"     { tokenise_l_c lexbuf }
  | "let"       { tokenise_l_c lexbuf }
  | "in"        { tokenise_l_c lexbuf }
  | "end"       { tokenise_l_c lexbuf }
  | "function"  { tokenise_l_c lexbuf }
  | "var"       { tokenise_l_c lexbuf }
  | "type"      { tokenise_l_c lexbuf }
  | "array"     { tokenise_l_c lexbuf }
  | "if"        { tokenise_l_c lexbuf }
  | "then"      { tokenise_l_c lexbuf }
  | "else"      { tokenise_l_c lexbuf }
  | "do"        { tokenise_l_c lexbuf }
  | "of"        { tokenise_l_c lexbuf }
  | "nil"       { tokenise_l_c lexbuf }
  | "int"       { tokenise_l_c lexbuf }
  | "string"    { tokenise_l_c lexbuf }
  | id          { tokenise_l_c lexbuf }
  | ','         { tokenise_l_c lexbuf }
  | ':'         { tokenise_l_c lexbuf }
  | ';'         { tokenise_l_c lexbuf }
  | '('         { tokenise_l_c lexbuf }
  | ')'         { tokenise_l_c lexbuf }
  | '['         { tokenise_l_c lexbuf }
  | ']'         { tokenise_l_c lexbuf }
  | '{'         { tokenise_l_c lexbuf }
  | '}'         { tokenise_l_c lexbuf }
  | '.'         { tokenise_l_c lexbuf }
  | '+'         { tokenise_l_c lexbuf }
  | '-'         { tokenise_l_c lexbuf }
  | '*'         { tokenise_l_c lexbuf }
  | '/'         { tokenise_l_c lexbuf }
  | '='         { tokenise_l_c lexbuf }
  | "<>"        { tokenise_l_c lexbuf }
  | '<'         { tokenise_l_c lexbuf }
  | "<="        { tokenise_l_c lexbuf }
  | '>'         { tokenise_l_c lexbuf }
  | ">="        { tokenise_l_c lexbuf }
  | '&'         { tokenise_l_c lexbuf }
  | '|'         { tokenise_l_c lexbuf }
  | ":="        { tokenise_l_c lexbuf }
  | eof
   {  let (l, c) = l_c lexbuf in
      ("EOF", l, c)
   }

and read_string buf lc =
  parse
  | '"'
    { let (l, c) = lc in
      (Buffer.contents buf, l , c) }
  | '\\' '/'    { Buffer.add_char buf '/'; read_string buf lc lexbuf }
  | '\\' '\\'   { Buffer.add_char buf '\\'; read_string buf lc lexbuf }
  | '\\' 'f'    { Buffer.add_char buf '\012'; read_string buf lc lexbuf }
  | '\\' 'n'    { Buffer.add_char buf '\n'; read_string buf lc lexbuf }
  | '\\' 'r'    { Buffer.add_char buf '\r'; read_string buf lc lexbuf }
  | '\\' 't'    { Buffer.add_char buf '\t'; read_string buf lc lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lc lexbuf
    }
  | _ {raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }

and read_comment buf nob lc =    (* nob: number of open braces *)
  parse
  | "*/"
    { if nob = 1
      then
        let (l, c) = lc in
        (Buffer.add_string buf "*/";
        (Buffer.contents buf, l, c))
      else
        (Buffer.add_string buf "*/";
        read_comment buf (nob - 1) lc lexbuf)
    }
  | "/*"
    {
      Buffer.add_string buf "/*";
      read_comment buf (nob + 1) lc lexbuf
    }
  | _
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_comment buf nob lc lexbuf
    }
  | eof
    { let (l, c) = lc in
      (Buffer.contents buf, l , c)
    }
