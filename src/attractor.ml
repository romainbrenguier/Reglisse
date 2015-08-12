let controllable_name name = 
     if String.length name > 13 then
       String.sub name 0 13 = "controllable_" 
     else false

let controllables aiger = 
  List.fold_left
    (fun (c,u) lit -> 
     let name,_ = Aiger.lit2symbol aiger lit in
     if controllable_name name then (lit :: c, u) 
     else (c, lit :: u)
    ) ([],[]) aiger.Aiger.inputs 

let controllable_variables aiger =
  let aux = List.map (AigerBdd.Variable.of_lit aiger) in
  let contr,uncontr = controllables aiger in
  aux contr, aux uncontr

let of_aiger aiger = 
  let c,u = controllables aiger in
  (aiger, c, u)
    
let read_from_file file = 
  let aiger = Aiger.read_from_file file in
  of_aiger aiger

let rename_substitute_restrict unsafe aiger =
  let renamed = AigerBdd.Circuit.rename_configuration (Region.latch_configuration unsafe) (AigerBdd.Circuit.array_variables aiger) (AigerBdd.Circuit.array_next_variables aiger) in
  let substitued = Cudd.bddVectorCompose renamed (AigerBdd.Circuit.composition_vector aiger) in
  Cudd.bddOr substitued (Region.latch_input_configuration unsafe)

let uncontrollable_predecessors_with_restriction unsafe aiger controllable_cube uncontrollable_cube weak restriction =
  let restricted = rename_substitute_restrict unsafe aiger in
  if weak 
  then 
    let exist_quantified = Cudd.bddExistAbstract (Cudd.bddAnd (Region.latch_configuration restriction) restricted) uncontrollable_cube in
    let univ_quantified = Cudd.bddUnivAbstract (Cudd.bddOr exist_quantified (Cudd.bddNot (Region.latch_configuration restriction))) controllable_cube in
    Region.of_bdds univ_quantified exist_quantified
  else
    let univ_quantified = Cudd.bddUnivAbstract (Cudd.bddOr restricted (Cudd.bddNot (Region.latch_configuration restriction))) controllable_cube in
    let exist_quantified = Cudd.bddExistAbstract (Cudd.bddAnd (Region.latch_input_configuration restriction) univ_quantified) uncontrollable_cube in
    Region.of_bdds exist_quantified univ_quantified

let trap_with_restriction aiger controllables uncontrollables ?(weak=false) unsafe restriction =
  let controllable_cube = AigerBdd.Variable.make_cube controllables in
  let uncontrollable_cube = AigerBdd.Variable.make_cube uncontrollables in

  let aux u = 
    Common.debug "in Attractor.trap_with_restriction: uncontrollable_predecessor step";
    uncontrollable_predecessors_with_restriction u aiger controllable_cube uncontrollable_cube weak restriction
  in 
  Region.greatest_fixpoint aux unsafe


let trap aiger contr uncontr ?(weak=false) unsafe =
  trap_with_restriction aiger contr uncontr ~weak (Region.of_bdds unsafe (Cudd.bddFalse())) (Region.tt())

let attractor_with_restriction aiger controllables uncontrollables ?(weak=false) safe restriction =
  trap_with_restriction aiger uncontrollables controllables ~weak:(not weak) safe restriction

let attractor aiger contr uncontr ?(weak=false) safe = 
  attractor_with_restriction aiger contr uncontr ~weak (Region.of_bdds safe (Cudd.bddFalse())) (Region.tt())

let strategy game region =
  Cudd.bddVectorCompose (AigerBdd.Circuit.rename_configuration
			   (Region.latch_configuration region)
			   (AigerBdd.Circuit.array_variables game) 
			   (AigerBdd.Circuit.array_next_variables game))
			(AigerBdd.Circuit.composition_vector game)


(** Returns an associative list of controllable input and BDD *)
let strategy_to_bdds strategy controllables uncontrollables = 
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
      (*List.iter (fun x -> Printf.printf "controllable : %d\n" (AigerBdd.Variable.to_int x)) contr;
      Printf.printf "writing strategy%d.dot ----------\n" (!cnt);
      Cudd.dumpDot ("strategy"^ string_of_int !cnt ^".dot") bdd;*)

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


let strategy_to_aiger aiger strategy controllables uncontrollables = 
  Timer.log "converting strategy to bdds";
  let strategy_bdds = strategy_to_bdds strategy controllables uncontrollables in
  Timer.log "converting bdds to a circuit";
  let circuit = 
    List.fold_left
      (fun aiger (contr,bdd) -> 
       input_output_from_bdd aiger contr bdd 
      ) aiger strategy_bdds 
  in
  Timer.log "cleaning the circuit";
  AigerBdd.reorder_aiger circuit


let test aiger = 
  let bdd_of_lit lit = 
    AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol (Aiger.lit2symbol aiger lit)))
  in

  let contr,uncontr = controllables aiger in
  let (contrv,uncontrv) = controllable_variables aiger in 
  if !Common.display_debug then List.iter (fun x -> Printf.printf "%d controllable\n" (Aiger.lit2int x)) contr;

  AigerBdd.init aiger;
  let unsafe = List.fold_left Cudd.bddOr (Cudd.bddFalse()) (List.map bdd_of_lit aiger.Aiger.outputs) in
  let initial = AigerBdd.bdd_of_valuation (AigerBdd.Circuit.initial_state aiger) in
  let precomputation = AigerBdd.Circuit.of_aiger aiger in   
  let trap_set = trap precomputation contrv uncontrv ~weak:false unsafe in
  let trap_set_weak = trap precomputation contrv uncontrv ~weak:true unsafe in
  let initial_losing = Cudd.bddRestrict (Region.latch_configuration trap_set) initial in
  let initial_losing_weak = Cudd.bddRestrict (Region.latch_configuration trap_set_weak) initial in

  if !Common.display_debug 
  then Cudd.dumpDot ("trap.dot") (Region.latch_input_configuration trap_set);

  if Cudd.value initial_losing = 1 
  then print_endline "unrealizable"
  else if Cudd.value initial_losing = 0
  then 
    (print_endline "realizable";
     let strat = strategy_to_aiger aiger (strategy precomputation (Region.negation trap_set)) contrv uncontrv in
     let file_name = Sys.argv.(1)^".controller.aag" in
     let outch = open_out file_name in
     Aiger.write strat outch;
     close_out outch;
     Printf.printf "controller written in file %s\n" file_name
    )
  else (Cudd.dumpDot ("problem_report.out") initial_losing;  print_endline "problem...");
  
  if Cudd.value initial_losing_weak = 1 
  then print_endline "unrealizable by a weak controller"
  else if Cudd.value initial_losing_weak = 0
  then print_endline "realizable by a weak controller"
  else (Cudd.dumpDot ("problem_report.out") initial_losing_weak;  print_endline "problem...")

let main = 
  (* Common.display_debug := true;*)
  if Filename.check_suffix Sys.argv.(0) "attractor" 
     || Filename.check_suffix Sys.argv.(0) "attractor.byte" 
  then 
    if Array.length Sys.argv < 2 
    then Printf.printf "usage : %s <file>\n" Sys.argv.(0)
    else 
      let aiger = Aiger.read_from_file Sys.argv.(1) in
      test aiger
