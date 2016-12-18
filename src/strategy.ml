module Aiger = AigerImperative

type t = Cudd.bdd

let of_region game region =
  let renamed = BddVariable.rename_configuration
			   (Region.latch_configuration region)
			   (Circuit.array_variables game) 
			   (Circuit.array_next_variables game)
  in
  let composition_vector = Circuit.composition_vector game in
  Cudd.bddVectorCompose renamed composition_vector


let all () = Cudd.bddTrue()
let none () = Cudd.bddFalse()
let disj list = 
  List.fold_left Cudd.bddOr (none()) list

let conj list = 
  List.fold_left Cudd.bddAnd (all()) list

let to_bdd strat = strat

let rename strat renaming =
  let var = renaming |> List.map fst
	 |> List.map BddVariable.find
	 |> Array.of_list in
  let nvar = renaming |> List.map snd
	 |> List.map BddVariable.find
	 |> Array.of_list in
  BddVariable.rename_configuration strat var nvar

(** Returns an associative list of controllable input and BDD *)
let to_bdds strategy controllables uncontrollables = 
  let cnt = ref 0 in
  let rec aux (bdd,accu) contr = 
    incr cnt;
    let bdd_inp = BddVariable.to_bdd contr in 
    let val_inp = 
      Cudd.bddNot (Cudd.bddExistAbstract (Cudd.bddAnd (Cudd.bddNot bdd_inp) bdd)
		     (BddVariable.make_cube controllables)) in

    try
      let bdd =  Cudd.bddExistAbstract 
		   (Cudd.bddAnd bdd (Cudd.bddOr 
				       (Cudd.bddAnd val_inp bdd_inp)
				       (Cudd.bddAnd (Cudd.bddNot val_inp)
					  (Cudd.bddNot bdd_inp))))
		   (BddVariable.make_cube [contr])
		   
      in 
      bdd, ((contr,val_inp) :: accu)
    with Not_found -> 
      prerr_endline ("warning: no symbol found for output symbol "^(BddVariable.to_string contr));
      bdd, accu
  in

  let bdd,assoc = List.fold_left aux (strategy,[]) controllables in
  assoc


let substitute src dst i =
  if i mod 2 = 1 
  then
    if Aiger.neg i = src then Aiger.neg dst else i
  else
    if i = src then dst else i


(* remove an input and replace it by the given gate *)
let input2gate aiger input gate =
  Printf.printf "input2gate %d %d\n" input gate;
    let sub = substitute input gate in
  let renumber x =
    if x > input + 1 then (x - 2) else x
  in
  let new_inputs = Aiger.LitSet.make() in
  let sym = Aiger.lit2string_exn aiger input in
  let new_symbols = Hashtbl.create aiger.Aiger.maxvar in
  let new_symbols_inv = Hashtbl.create aiger.Aiger.maxvar in
  let new_latches = Hashtbl.create aiger.Aiger.num_latches in
  let new_ands = Hashtbl.create aiger.Aiger.num_ands in
  let new_outputs = Aiger.LitSet.make() in
  Aiger.LitSet.iter
    (fun l ->
      if l <> input then Aiger.LitSet.add new_inputs (renumber l)
    ) aiger.Aiger.inputs ;
  Hashtbl.iter (fun k i -> Hashtbl.add new_symbols (renumber k) i) aiger.Aiger.symbols ;
  Hashtbl.iter (fun i k -> Hashtbl.add new_symbols_inv i (renumber k)) aiger.Aiger.symbols_inv ;
  Hashtbl.iter (fun l r -> Hashtbl.add new_latches (renumber l) (renumber (sub r))) aiger.Aiger.latches;
  Hashtbl.iter (fun lhr (rhs0,rhs1) ->
    Hashtbl.add new_ands (renumber lhr) (renumber (sub rhs0),renumber (sub rhs1))
  ) aiger.Aiger.ands;
  Aiger.LitSet.iter (fun l -> Aiger.LitSet.add new_outputs (renumber l)) aiger.Aiger.outputs;
  {aiger with 
    Aiger.num_inputs = aiger.Aiger.num_inputs - 1 ; 
    Aiger.inputs = new_inputs ; 
    Aiger.latches = new_latches ; 
    Aiger.ands = new_ands;
    Aiger.symbols = new_symbols;
    Aiger.symbols_inv = new_symbols_inv;
    Aiger.outputs = new_outputs;
    Aiger.maxvar = aiger.Aiger.maxvar - 1;
  }, renumber gate


(* val input_output_from_bdd : Aiger.t -> Aiger.variable -> Cudd.bdd -> Aiger.t 
   Replace the input by the BDD, and also put it as an output
*)
let input_output_from_bdd aiger input bdd =
  Printf.printf "input_output_from_bdd %d\n" (BddVariable.to_int input);
  let map = AigerImpBdd.map_of_aiger aiger in
  let input_lit = AigerImpBdd.VariableMap.find input map in
  let sym = Aiger.lit2string_exn aiger input_lit in
  let gate = AigerImpBdd.add_bdd_to_aiger aiger AigerImpBdd.VariableMap.empty bdd in
  Printf.printf "input_output_from_bdd %s <- %d %s %d\n" (BddVariable.to_string input) input_lit sym gate;
  Aiger.write stdout aiger;
  let aiger,gate = input2gate aiger input_lit gate in
  Aiger.set_output aiger sym gate;
  try
    Aiger.write stdout aiger;
    let a = Aiger.order_gates_exn aiger in
    Aiger.write stdout a; a
  with (Aiger.Circular_circuit i) ->
    Printf.printf "CircularCircuit(%d)\n" i;
    raise (Aiger.Circular_circuit i);
    aiger


let to_aiger aiger strategy controllables uncontrollables = 
  Message.log "converting strategy to bdds";
  let strategy_bdds = to_bdds strategy controllables uncontrollables in
  Message.log "converting bdds to a circuit";
  List.fold_left
    (fun aig (contr,bdd) ->
      input_output_from_bdd aig contr bdd;
    ) aiger strategy_bdds;

(*Message.log "cleaning the circuit";
  AigerBdd.reorder_aiger aiger*)

