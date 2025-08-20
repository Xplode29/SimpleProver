
let send_to_z3 formula = 
  let vars = Utils.get_vars_from_formula formula in
  let message = 
    Lemmas.prefix ^
    (List.fold_left (fun acc loc -> Printf.sprintf "(declare-const %s Int)\n%s" loc acc) "" (Utils.remove_duplicates vars)) ^
    Printf.sprintf "(assert %s)\n" (Smtlib_converter.smtlib_of_formula formula) ^
    "(check-sat)\n(get-model)"
  in
  
  let oc = open_out "output/smtlib.smt2" in
  Printf.fprintf oc "%s\n" message;
  close_out oc;

  Utils.run_terminal "z3 output/smtlib.smt2"