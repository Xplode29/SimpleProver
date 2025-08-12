
let send_to_z3 formula = 
  let message = 
    "(set-logic ALL)\n" ^
    (List.fold_left (fun acc loc -> Printf.sprintf "(declare-const %s Int)\n%s" loc acc) "" (Utils.remove_duplicates (Utils.get_vars_from_formula formula))) ^
    Printf.sprintf "(assert %s)\n" (Z3_converter.z3_of_formula formula) ^
    "(check-sat)\n(get-model)"
  in
  
  let oc = open_out "output/z3" in
  Printf.fprintf oc "%s\n" message;
  close_out oc;

  Utils.run_terminal "z3 output/z3"