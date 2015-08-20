let correct_strategy p strat controllables uncontrollables =
  let controllable_cube = AigerBdd.Variable.make_cube controllables in
  let uncontrollable_cube = AigerBdd.Variable.make_cube uncontrollables in
  let variables_cube = AigerBdd.Variable.make_cube (AigerBdd.VariableSet.elements (AigerBdd.Circuit.variables p)) in
  let exist_quantified = Cudd.bddExistAbstract strat controllable_cube in
  print_endline "univ quantification over uncontrollable";
  let univ_quantified = Cudd.bddUnivAbstract exist_quantified uncontrollable_cube in
  print_endline "univ quantification over variables";
  let res = Cudd.bddUnivAbstract univ_quantified variables_cube in
  print_endline "result";
  res

let check_strategies p strat controllables uncontrollables u =
  let controllable_cube = Cudd.make_cube (List.map Aiger.lit2int controllables) in
  let uncontrollable_cube = Cudd.make_cube (List.map Aiger.lit2int uncontrollables) in
  let unsafe_pred u =
    let renamed = AigerBdd.Circuit.rename_configuration u (AigerBdd.Circuit.array_variables p) (AigerBdd.Circuit.array_next_variables p) in
    let substitued = Cudd.bddVectorCompose renamed (AigerBdd.Circuit.composition_vector p) in
    let apply_strat = Cudd.bddAnd substitued strat in
    let exist_quantified = Cudd.bddExistAbstract apply_strat controllable_cube in
    let exist_quantified1 = Cudd.bddExistAbstract exist_quantified uncontrollable_cube in
    exist_quantified1
  in
  let rec trap u = 
    let x = unsafe_pred u in
    let res = Cudd.bddOr u x in
    if Cudd.equal res u then res else trap res
  in trap u 

let assume_admissible game player1_inputs player2_inputs fail1 fail2 = 
  let v1 = value game player1_inputs player2_inputs fail1 in
  let adm1 = strategies game v1 in
  let v2 = value game player2_inputs player1_inputs ~weak:true fail2 in
  let adm2 = strategies game v2 in
  let w1 = Region.negation
    (Attractor.trap game player1_inputs player2_inputs (*Region.intersection adm1 adm2*) fail1) in
  let w2 = Region.negation
    (Attractor.trap game player2_inputs player1_inputs (*Region.intersection adm2 adm1*) ~weak:true fail2) in
  Region.intersection w1 w2


let safety_synthesis aiger inputs outputs =
  let contr,uncontr = control aiger inputs outputs in
  (*let v = AigerBdd.Variable.find (AigerBdd.of_aiger_symbol ("never_accept",Some 0)) in*)

  (*let bdd_of_lit lit = 
    AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol (Aiger.lit2symbol aiger lit)))
  in*)
  let unsafe = AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol ("never_accept",Some 0))) in

  let aigerBdd = AigerBdd.Circuit.of_aiger aiger in
  let losing = Attractor.trap aigerBdd contr uncontr unsafe in
  let winning = Region.negation losing in

  let initial = Region.initial_state aiger in
  let initial_winning = Cudd.bddRestrict (Region.latch_configuration winning) initial in

  if Cudd.value initial_winning = 1 
  then print_endline "realizable"
  else if Cudd.value initial_winning = 0
  then failwith "unrealizable"
  else 
    (print_endline "problem determining if the initial state is winning";
     print_endline "writing BDDs in initial_winning.out initial_state.out and winning.out";
     Cudd.dumpDot ("initial_winning.out") initial_winning;
     Cudd.dumpDot ("initial_state.out") initial;
     Cudd.dumpDot ("winning.out") (Region.latch_configuration winning);
    );

  let strategy = Attractor.strategy aigerBdd winning in
  let aig_strategy = Attractor.strategy_to_aiger aiger strategy contr uncontr in
  aig_strategy
    
(*
let synthesis aiger inputs outputs = 
  print_endline "warning: for now reachability conditions are not working";
  let formula = WeakMuller.Formula.F_temp (WeakMuller.Formula.TF_always (StateFormula.AF_equal ("never_accept", 0))) in
  (*WeakMuller.Formula.F_and
      (WeakMuller.Formula.F_temp (WeakMuller.Formula.TF_eventually (StateFormula.AF_equal ("eventually_accept", 1))),
       WeakMuller.Formula.F_temp (WeakMuller.Formula.TF_always (StateFormula.AF_equal ("never_accept", 0)))
      )
  in*) 

  let contr,uncontr =  control aiger inputs outputs in
  let product, winning = WeakMuller.winning_region aiger contr uncontr formula false in
  print_endline "winning";
  let initial = Region.initial_state product in
  let initial_winning = Cudd.bddRestrict (Region.latch_configuration winning) initial in

  if Cudd.value initial_winning = 1 
  then print_endline "realizable"
  else if Cudd.value initial_winning = 0
  then print_endline "unrealizable"
  else Cudd.dumpDot ("initial_winning.out") initial_winning  
*)

    match specs with 
    | [spec] -> 
      let aiger = reglisse_to_aiger spec in
      if !output_game
      then (print_endline ("writing "^file^"_game.aag"); Aiger.write_to_file aiger (file^"_game.aag"));
      safety_synthesis aiger spec.inputs spec.outputs
    | spec_list ->
