open Core.Utils
open Core.Ast
open Parser.Main

let rec verif_vcs = function
  |[] -> print_endline "End of verification conditions."
  |h::t -> 
    print_endline (string_of_formula h);
    print_endline (Solver_api.Sender.send_to_z3 (NotF h)); 
    verif_vcs t

let () = 
  if Array.length Sys.argv <> 2 then failwith "Usage: ./main <filename>";
  let filename = Sys.argv.(1) in
  let pg = parse (read_file filename) in

  let (wp, vcs) = Prover.Vc_gen.wp pg.body pg.postcondition in

  print_endline (string_of_program pg);
  print_endline "" ;

  print_endline "VCs : ";
  verif_vcs vcs;
  print_endline "" ;

  print_endline "Weakest precondition : ";
  print_endline (Core.Utils.string_of_formula wp);
  print_endline (Solver_api.Sender.send_to_z3 (NotF (ImplyF (pg.precondition, wp))));
  print_endline "";