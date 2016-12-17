open Common

let display_latches = false

let successors game valuation inputs = 
  let bdd_val = AigerBdd.bdd_of_valuation valuation in
  let bdd_inp = AigerBdd.bdd_of_valuation inputs in
  let bdd = Cudd.bddAnd bdd_val bdd_inp in
  let update = AigerBdd.Circuit.updates game in
  let new_val = 
    AigerBdd.VariableMap.fold
      (fun var _ accu -> 
       let update = Hashtbl.find update var in
       let res = Cudd.bddRestrict update bdd in
       if Cudd.equal res (Cudd.bddTrue()) 
       then AigerBdd.VariableMap.add var true accu
       else if Cudd.equal res (Cudd.bddFalse()) 
       then AigerBdd.VariableMap.add var false accu
       else 
	 (Cudd.dumpDot "value.dot" res;
	  Cudd.dumpDot "update.dot" res;
	  failwith "In Simulation.successors: undefined value")
      ) valuation AigerBdd.VariableMap.empty
  in new_val


let input_inputs_compact =  
  Printf.printf "%s [%d,%d] ?\n> " name 0 (exp size - 1);
  let b = ref (read_int ()) in 
  List.fold_left
    (fun state name -> 
      let size = Aiger.size_symbol aiger name in
      let rec loop state i = 
	if i = size then state 
	else
	  let new_state = 
	    AigerBdd.VariableMap.add (AigerBdd.Variable.find (name,i)) ((b lsr i) mod 2 = 1) state
	  in loop new_state (i+1)

      in loop state 0
    ) 
    AigerBdd.VariableMap.empty
    (Aiger.inputs aiger)

let input_inputs aiger =
  List.fold_left
    (fun state name -> 
      let size = Aiger.size_symbol aiger name in
      Printf.printf "%s [%d,%d] ?\n> " name 0 (exp size - 1);
      let b = read_int () in 
      let rec loop state i = 
	if i = size then state 
	else
	  let new_state = 
	    AigerBdd.VariableMap.add (AigerBdd.Variable.find (name,i)) ((b lsr i) mod 2 = 1) state
	  in loop new_state (i+1)

      in loop state 0
    ) 
    AigerBdd.VariableMap.empty
    (Aiger.inputs aiger)

 
let main =
  if Array.length Sys.argv < 2 then prerr_endline "usage: simulation <aiger file>";
  Printf.printf "reading from %s\n" Sys.argv.(1);
  let aiger = Aiger.read_from_file Sys.argv.(1) in
  AigerBdd.init aiger; 
  
  let state = 
    AigerBdd.valuation_of_list 
      (List.fold_left 
	 (fun accu name -> 
	  let literals = Aiger.name_to_literals aiger name in
	  let variables = 
	    Array.mapi 
	      (fun i lit -> 
	       let v = AigerBdd.Variable.find (name,i) 
					      (*try 
		 with x -> if i = 0 then AigerBdd.Variable.find (name,None) else raise x*)
	       in (v,false)
	      ) literals
	  in List.rev_append (Array.to_list variables) accu
	 ) [] (List.rev_append (Aiger.latches aiger) (Aiger.outputs aiger))
      )
  in

  let game = AigerBdd.Circuit.of_aiger aiger in

  let outputs_latches = 
    if display_latches 
    then List.rev_append (Aiger.outputs aiger) (Aiger.latches aiger) 
    else Aiger.outputs aiger
  in

  AigerBdd.Circuit.print_valuation aiger outputs_latches state;
  let rec loop state = 
    try 
      let inputs = input_inputs aiger in
      let new_state = successors game state inputs in
      AigerBdd.Circuit.print_valuation aiger outputs_latches new_state;
      loop new_state
    with
    | End_of_file -> state
    | Failure x -> Printf.printf "exiting (%s)\n" x; state
  in

  loop state


(*  let valuation = 
    match AigerBdd.bdd_to_valuations state with 
    | a :: _ -> a
    | _ -> failwith "no successor"
    (*List.rev_append (Aiger.outputs aiger) (Aiger.latches aiger)*)
  in
 *)
