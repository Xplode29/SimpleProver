
let read_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content

let parse string =
  let lexbuf = Lexing.from_string string in
  try
    let result = Parsing.program Lexer.read lexbuf in result
  with
  | e ->
    let pos = lexbuf.lex_curr_p in
    Printf.eprintf "Error at line %d, column %d: %s\n"
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) (Printexc.to_string e);
    let start_pos = max 0 (pos.pos_cnum - 20) in
    let end_pos = min (String.length string) (pos.pos_cnum + 20) in
    let context = String.sub string start_pos (end_pos - start_pos) in
    Printf.eprintf "Error context: %s\n" context;
    print_endline "Please check your input file.";
    exit (-1)