module VariableSet = Set.Make(BddVariable)
module VariableMap = AigerImpBdd.VariableMap
module Aiger = AigerImperative
module AigerBdd = AigerImpBdd

type t = 
  {
    updates:(BddVariable.t , Cudd.bdd) Hashtbl.t;
    variables: VariableSet.t;
    next_variables: VariableSet.t;
    map: AigerImperative.lit VariableMap.t;
    array_variables: BddVariable.t array;
    array_next_variables: BddVariable.t array;
    composition_vector: Cudd.bdd array;
  }


let updates p = p.updates
let variables p = p.variables
let next_variables p = p.next_variables
let array_variables p = p.array_variables
let array_next_variables p = p.array_next_variables
let composition_vector p = p.composition_vector
let map p = p.map

exception Undefined of string 



(** compute updates in a table, all variables are even *)
let compute_updates aiger =
  let gate_bdd = Hashtbl.create aiger.Aiger.maxvar in
  let updates = Hashtbl.create aiger.Aiger.num_latches in
  Hashtbl.add gate_bdd Aiger.aiger_false (Cudd.bddFalse ());
  Hashtbl.add gate_bdd Aiger.aiger_true (Cudd.bddTrue ());

  Aiger.LitSet.fold 
    (fun _ inp () -> 
      let var = BddVariable.find (Aiger.lit2string_exn aiger inp) in
      let bdd = BddVariable.to_bdd var in
      Hashtbl.add gate_bdd inp bdd;
      Hashtbl.add gate_bdd (Aiger.neg inp) (Cudd.bddNot bdd)
    ) aiger.Aiger.inputs ();
  
  Hashtbl.iter 
    (fun l _ -> 
      let var = BddVariable.find (Aiger.lit2string_exn aiger l) in
      let bdd = BddVariable.to_bdd var in
      Hashtbl.add gate_bdd l bdd;
      Hashtbl.add gate_bdd (Aiger.neg l) (Cudd.bddNot bdd)
    ) aiger.Aiger.latches;

  List.iter 
    (fun (g,l,r) -> 
      try
	let gl = Hashtbl.find gate_bdd l in
	let gr = Hashtbl.find gate_bdd r in
	let b = Cudd.bddAnd gl gr in
	Hashtbl.add gate_bdd g b;
	Hashtbl.add gate_bdd (Aiger.neg g) (Cudd.bddNot b)
      with Not_found -> 
	Printf.eprintf "warning in Circuit.compute_updates: gate not found\n";
	raise Not_found
    ) (Aiger.gates aiger);

  Hashtbl.iter 
    (fun l n -> 
      try 
	let bdd = Hashtbl.find gate_bdd n in
	Hashtbl.add updates (BddVariable.find (Aiger.lit2string_exn aiger l)) bdd
      with Not_found -> 
	Printf.eprintf "warning in Circuit.compute_updates: gate not found\n";
	raise Not_found 
    ) aiger.Aiger.latches;

  Aiger.LitSet.iter 
    (fun lit -> 
      try 
	(* Warning: several output can have the same litteral *)
	let bdd = Hashtbl.find gate_bdd lit in
	(* Warning an output could have the same name than a latch *)
	let bdd_var = BddVariable.find (Aiger.lit2string_exn aiger lit) in
	Printf.printf "adding bdd for ouput %s\n" (BddVariable.to_string bdd_var);
	Cudd.dumpDot "update_output.dot" bdd;
	Hashtbl.add updates bdd_var bdd
      with Not_found  -> 
	Printf.eprintf "warning in Circuit.compute_uptades: gate %d not found\n" lit;
	raise Not_found
    ) aiger.Aiger.outputs;
  updates


let variables_aiger aiger = 
  (* set of variables *)
  let vs = VariableSet.empty in

  let vs =
    Aiger.LitSet.fold
      (fun i inp vs ->
	VariableSet.add (BddVariable.find (Aiger.lit2string_exn aiger inp)) vs
      ) aiger.Aiger.inputs vs
  in
  let vs =
    Aiger.LitSet.fold
      (fun i out vs ->
	VariableSet.add (BddVariable.find (Aiger.lit2string_exn aiger out)) vs
      ) aiger.Aiger.outputs vs
  in
  let vs = 
    Hashtbl.fold (fun l _ vs -> 
      VariableSet.add (BddVariable.find (Aiger.lit2string_exn aiger l)) vs
    ) aiger.Aiger.latches vs
  in vs


let of_aiger aiger =
  print_endline "Circuit of AIG:";
  Aiger.write stdout aiger;    
  let updates = compute_updates aiger in
  let variables = variables_aiger aiger in
  let next_variables = VariableSet.fold (fun x accu -> VariableSet.add (BddVariable.next x) accu) variables VariableSet.empty in
  let array_variables = Array.of_list (VariableSet.elements variables) in
  let array_next_variables = Array.of_list (VariableSet.elements next_variables) in
  let composition_vector = 
    Array.init (* (aiger.Aiger.maxvar * 2 + 2) (* should it really be this value ? *)*)
      (BddVariable.max_var ())
      (fun i -> 
	try Hashtbl.find updates (BddVariable.of_int (i-1))
	with Not_found -> Cudd.bddTrue())
  in
  let map = AigerBdd.map_of_aiger aiger in
  { updates = updates; variables=variables; next_variables = next_variables;
    array_variables=array_variables; array_next_variables = array_next_variables;
    composition_vector=composition_vector; map=map}


let print_valuation aiger names valuation = 
  List.iter
    (fun name ->
      failwith "not implemented"
      (*let size = Aiger.size_symbol aiger name in
      let value = ref 0 in
      for i = size - 1 downto 0 do
	(value := 2 * !value + 
	   (if BddVariableMap.find (BddVariable.find (name,i)) valuation
	    then 1 else 0));
	(*Printf.printf "%s.(%d) (= var %d): %b\n" name i (BddVariable.to_int (BddVariable.find (name,i))) (BddVariableMap.find (BddVariable.find (name,i)) valuation);*)
	
      done;
      Printf.printf "%s = %d\n" name !value*)
    ) names
    

let initial_state aiger =
  AigerBdd.valuation_of_list 
    (List.fold_left 
       (fun accu name -> 
	   failwith "unimplemented"
(*	 let literals = Aiger.name_to_literals aiger name in
	 let variables = 
	     (*Array.mapi 
	     (fun i lit -> 
	       let v = BddVariable.find (name,i) 
	       in (v,false)
	     ) literals*)
	 in List.rev_append (Array.to_list variables) accu*)
       ) [] (List.rev_append (Aiger.latches aiger) (Aiger.outputs aiger))
    )
    
