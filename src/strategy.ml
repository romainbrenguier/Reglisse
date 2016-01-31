type t = Cudd.bdd

let of_region game region =
  let renamed = AigerBdd.Circuit.rename_configuration
			   (Region.latch_configuration region)
			   (AigerBdd.Circuit.array_variables game) 
			   (AigerBdd.Circuit.array_next_variables game)
  in
  let composition_vector = AigerBdd.Circuit.composition_vector game in
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
	 |> List.map (fun x -> AigerBdd.Variable.find (x,0))
	 |> Array.of_list in
  let nvar = renaming |> List.map snd
	 |> List.map (fun x -> AigerBdd.Variable.find (x,0)) 
	 |> Array.of_list in
  AigerBdd.Circuit.rename_configuration strat var nvar

(** Returns an associative list of controllable input and BDD *)
let to_bdds strategy controllables uncontrollables = 
  let cnt = ref 0 in
  let rec aux (bdd,accu) contr = 
    incr cnt;
    let bdd_inp = AigerBdd.Variable.to_bdd contr in 
    let val_inp = 
      Cudd.bddNot (Cudd.bddExistAbstract (Cudd.bddAnd (Cudd.bddNot bdd_inp) bdd) (AigerBdd.Variable.make_cube controllables)) in

    try
      let bdd =  Cudd.bddExistAbstract 
		   (Cudd.bddAnd bdd (Cudd.bddOr 
				       (Cudd.bddAnd val_inp bdd_inp)
				       (Cudd.bddAnd (Cudd.bddNot val_inp) (Cudd.bddNot bdd_inp))))
		   (AigerBdd.Variable.make_cube [contr])
		   
      in 
      bdd, ((contr,val_inp) :: accu)
    with Not_found -> 
      prerr_endline ("warning: no symbol found for output symbol "^(AigerBdd.Variable.to_string contr));
      bdd, accu
  in

  let bdd,assoc = List.fold_left aux (strategy,[]) controllables in
  assoc


let substitute src dst i = 
  if Aiger.strip i = src 
  then 
    if Aiger.sign i then Aiger.aiger_not dst else dst
  else i

(* remove an input and replace it by the given gate *)
let input2gate aiger input gate =
  let sub = substitute input gate in
  let renumber x = if Aiger.lit2int x > Aiger.lit2int input + 1 then Aiger.int2lit (Aiger.lit2int x - 2) else x in
  let inputs = List.filter (fun x -> x <> input) aiger.Aiger.inputs in
  let inputs = List.map renumber inputs in
  let sym = Aiger.lit2symbol aiger input in
  let abstract = Aiger.LitMap.remove input aiger.Aiger.abstract in
  let abstract = Aiger.LitMap.fold (fun k i accu -> Aiger.LitMap.add (renumber k) i accu) abstract Aiger.LitMap.empty in
  let symbols = Aiger.SymbolMap.remove sym aiger.Aiger.symbols in
  let symbols = Aiger.SymbolMap.map renumber symbols in
  let latches = List.map (fun (l,r) -> (renumber l, renumber (sub r))) aiger.Aiger.latches in
  let ands = List.map 
	       (fun (lhr,rhs0,rhs1) ->  (renumber lhr,renumber (sub rhs0),renumber (sub rhs1))
	       ) aiger.Aiger.ands
  in
  let outputs = List.map renumber aiger.Aiger.outputs in
  {aiger with 
    Aiger.num_inputs = aiger.Aiger.num_inputs - 1 ; 
    Aiger.inputs = inputs ; 
    Aiger.latches = latches ; 
    Aiger.ands = ands;
    Aiger.symbols = symbols;
    Aiger.abstract = abstract;
    Aiger.outputs = outputs;
    Aiger.maxvar = aiger.Aiger.maxvar - 1;
  },renumber gate

(* val input_output_from_bdd : Aiger.t -> Aiger.variable -> Cudd.bdd -> Aiger.t 
Replace the input by the BDD, and also put it as an output
*)
let input_output_from_bdd aiger input bdd =
  let map = AigerBdd.map_of_aiger aiger in
  let input_lit = AigerBdd.VariableMap.find input map in
  let sym = Aiger.lit2symbol aiger input_lit in
  let aiger,gate = AigerBdd.add_bdd_to_aiger aiger AigerBdd.VariableMap.empty bdd in
  let aiger,gate = input2gate aiger input_lit gate in
  Aiger.add_output aiger gate sym


let to_aiger aiger strategy controllables uncontrollables = 
  Timer.log "converting strategy to bdds";
  let strategy_bdds = to_bdds strategy controllables uncontrollables in
  Timer.log "converting bdds to a circuit";
  let circuit = 
    List.fold_left
      (fun aiger (contr,bdd) -> 
       input_output_from_bdd aiger contr bdd 
      ) aiger strategy_bdds 
  in
  Timer.log "cleaning the circuit";
  AigerBdd.reorder_aiger circuit

