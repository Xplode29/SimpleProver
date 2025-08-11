open Core.Utils
open Parser.Main

let () = 
  if Array.length Sys.argv <> 2 then failwith "Usage: ./main <filename>";
  let filename = Sys.argv.(1) in
  let ast = parse (read_file filename) in
  print_endline (string_of_program ast);