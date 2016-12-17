open ReglisseCommon

let display_latches = true
(*let display_latches = false*)

let successors game valuation inputs = 
  let bdd_val = AigerImpBdd.bdd_of_valuation valuation in
  let bdd_inp = AigerImpBdd.bdd_of_valuation inputs in
  let bdd = Cudd.bddAnd bdd_val bdd_inp in
  let update = AigerImpBdd.Circuit.updates game in
  print_endline "updates";
  Hashtbl.iter (fun k _ -> print_endline (AigerImpBdd.Variable.to_string k)) update;
  let new_val =
    Hashtbl.fold 
      (fun var update accu ->
	Printf.printf "looking for %s\n" (AigerImpBdd.Variable.to_string var);
	let res = Cudd.bddRestrict update bdd in
	if Cudd.equal res (Cudd.bddTrue()) 
	then AigerImpBdd.VariableMap.add var true accu
	else if Cudd.equal res (Cudd.bddFalse()) 
	then AigerImpBdd.VariableMap.add var false accu
	else 
	  (Cudd.dumpDot "value.dot" res;
	   Cudd.dumpDot "update.dot" res;
	   failwith "In Simulation.successors: undefined value")
      ) update AigerImpBdd.VariableMap.empty
  in new_val

  

let input_inputs aiger =
  List.fold_left
    (fun state name ->
      Printf.printf "%s ?\n> " name;
      let b = read_int () in 
      AigerImpBdd.VariableMap.add (AigerImpBdd.Variable.find name) (not (b=0))  state
    ) 
    AigerImpBdd.VariableMap.empty
    (Aiger.inputs aiger)

    
let print_valuation aiger names valuation = 
  List.iter
    (fun name ->
      let value =
	let var =
	  try AigerImpBdd.Variable.find (AigerImpBdd.of_aiger_symbol (name,None))
	  with Not_found ->AigerImpBdd.Variable.find (AigerImpBdd.of_aiger_symbol (name,Some 0))
	in
	try
	  if AigerImpBdd.VariableMap.find var valuation
	  then "1" else "0"
	with Not_found -> "unknown"
      in Printf.printf "%s = %s\n" name value
    ) names

let main =
  if Array.length Sys.argv < 2 then prerr_endline "usage: simulation <aiger file>";
  Printf.printf "reading from %s\n" Sys.argv.(1);
  let aiger = Aiger.read_from_file Sys.argv.(1) in
  AigerImpBdd.init aiger; 

  Printf.printf "reading state\n";
  let state = 
    AigerImpBdd.valuation_of_list 
      (List.fold_left 
	 (fun accu name ->
	   print_endline name;
	   let v = AigerImpBdd.Variable.find (try name with Not_found -> name^"<0>")in
	   (v,false) :: accu
       ) [] (List.rev_append (Aiger.latches aiger) (Aiger.outputs aiger))
      )
  in

  Printf.printf "making circuit\n";
  let game = AigerImpBdd.Circuit.of_aiger aiger in

  Printf.printf "looking at latches\n";
  let outputs_latches = 
    if display_latches 
    then List.rev_append (Aiger.outputs aiger) (Aiger.latches aiger) 
    else Aiger.outputs aiger
  in

  Printf.printf "printing valuation\n";
  print_valuation aiger outputs_latches state;
  let rec loop state = 
    try 
      let inputs = input_inputs aiger in
      print_endline "computing successors";
      let new_state = successors game state inputs in
      print_endline "new state computed";
      print_valuation aiger outputs_latches new_state;
      print_endline "end of valuation";
      loop new_state
    with
    | End_of_file -> state
    | Failure x -> Printf.printf "exiting (%s)\n" x; state
  in

  loop state


(*  let valuation = 
    match AigerImpBdd.bdd_to_valuations state with 
    | a :: _ -> a
    | _ -> failwith "no successor"
    (*List.rev_append (Aiger.outputs aiger) (Aiger.latches aiger)*)
  in
 *)
