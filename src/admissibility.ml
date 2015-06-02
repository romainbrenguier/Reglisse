open Attractor

type value = Winning | CoopWinning | Help | Losing | NotWinning | NotLosing

(* Values have the following meaning:
 * 1 -> there is a winning strategy
 * 0 -> there is no winning strategy but we can win if other players cooperate
 * -1 -> it is impossible to win
 * 
 * help -> value is 0 but the adversary can take two different actions which result in a state of value at least 0
 *)

let winning p controllables uncontrollables ?(weak=false) unsafe = 
  Region.negation (trap p controllables uncontrollables ~weak unsafe)


let losing p controllables uncontrollables unsafe = 
  trap p (List.rev_append controllables uncontrollables) [] unsafe

let value p controllables uncontrollables ?(weak=false) unsafe = 
  print_endline "controllables";
  List.iter print_endline (List.map AigerBdd.Variable.to_string controllables);
  let winning_region = winning p controllables uncontrollables ~weak unsafe in
  print_endline "writing winning1.dot";
  Cudd.dumpDot "winning1.dot" (Region.latch_configuration winning_region);
  let losing_region = losing p controllables uncontrollables unsafe in
  let not_winning_region = Region.negation winning_region in
  let not_losing_region = Region.negation losing_region in
  let coop_winning_region = Region.intersection not_winning_region not_losing_region 
  in
  function 
  | Winning -> winning_region
  | Losing -> losing_region
  | CoopWinning -> coop_winning_region
  | NotWinning -> not_winning_region
  | NotLosing -> not_losing_region
     

let make algo game contr uncontr =
  let winning = algo game contr uncontr in
  let losing = Region.negation (algo game (List.rev_append uncontr contr) []) in
  let not_winning = Region.negation winning in
  let not_losing = Region.negation losing in
  function 
  | Winning -> winning
  | Losing -> losing
  | NotWinning -> not_winning
  | NotLosing -> not_losing
  | CoopWinning -> Region.intersection not_winning not_losing
  | Help -> failwith "Help states not implemented"


let strategies_with_restriction restriction p v =
  Printf.printf "warning restriction not used\n";
  let successor_not_winning = Cudd.bddVectorCompose (Cudd.bddSwapVariables (Region.latch_input_configuration (v NotWinning)) (AigerBdd.array_variables p) (AigerBdd.array_next_variables p)) (AigerBdd.composition_vector p) in
  let successor_losing = Cudd.bddVectorCompose (Cudd.bddSwapVariables (Region.latch_configuration (v Losing)) (AigerBdd.array_variables p) (AigerBdd.array_next_variables p)) (AigerBdd.composition_vector p) in
  Region.intersection 
    (Region.negation (Region.intersection_bdd (v Winning) successor_not_winning))
    (Region.negation (Region.intersection_bdd (v NotLosing) successor_losing))
(*  Cudd.bddAnd 
    (Cudd.bddNot (Cudd.bddAnd (Region.latch_configuration (v Winning)) successorNotWinning))
    (Cudd.bddNot (Cudd.bddAnd (Region.latch_configuration (v NotLosing)) successorLosing))
*)

let strategies x = strategies_with_restriction (Region.tt()) x

let strongly_winning_strategy p winning = failwith "unimplemented"

let correct_strategy p strat controllables uncontrollables =
  let controllable_cube = AigerBdd.Variable.make_cube controllables in
  let uncontrollable_cube = AigerBdd.Variable.make_cube uncontrollables in
  let variables_cube = AigerBdd.Variable.make_cube (AigerBdd.VariableSet.elements (AigerBdd.variables p)) in
  let exist_quantified = Cudd.bddExistAbstract strat controllable_cube in
  let univ_quantified = Cudd.bddUnivAbstract exist_quantified uncontrollable_cube in
  Cudd.bddUnivAbstract univ_quantified variables_cube

let check_strategies p strat controllables uncontrollables u =
  let controllable_cube = Cudd.make_cube (List.map Aiger.lit2int controllables) in
  let uncontrollable_cube = Cudd.make_cube (List.map Aiger.lit2int uncontrollables) in
  let unsafe_pred u =
    let renamed = Cudd.bddSwapVariables u (AigerBdd.array_variables p) (AigerBdd.array_next_variables p) in
    let substitued = Cudd.bddVectorCompose renamed (AigerBdd.composition_vector p) in
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

let compositional_synthesis game_list =
  let admissibles = 
    List.fold_left 
      (fun accu (a,g,c,u,err) ->
	Aiger.write_to_file a "game.aag";
	let v = value g c u err in
	let test = Region.negation (Attractor.trap g c u err) in
	print_endline "writing unsafe.dot";
	Cudd.dumpDot "unsafe.dot" err;
	print_endline "writing value1.dot";
	Cudd.dumpDot "value1.dot" (Region.latch_configuration (v Winning));
	print_endline "writing neg_trap.dot";
	Cudd.dumpDot "neg_trap.dot" (Region.latch_configuration test);
	print_endline "writing value0.dot";
	Cudd.dumpDot "value0.dot" (Region.latch_configuration (v CoopWinning));
	print_endline "writing value-1.dot";
	Cudd.dumpDot "value-1.dot" (Region.latch_configuration (v Losing));
	strategies g v :: accu
      ) [] game_list
  in
  print_endline "writing adm.dot";
  Cudd.dumpDot "adm.dot" (Region.latch_input_configuration (List.hd admissibles ));

  let conjunction = 
    List.fold_left Region.intersection (Region.tt()) admissibles
  in
  let product = 
    print_endline "we should be carefull that the output has the same name in both cases";
    List.fold_left 
      (fun accu (a,_,_,_,_) -> 
	Aiger.compose accu a
      ) Aiger.empty game_list
  in
  let product_bdd = AigerBdd.of_aiger product in
  let winning =
    List.fold_left
      (fun accu (a,g,c,u,err) ->
	let v = Attractor.trap product_bdd c u (*conjunction*) err in
	Region.intersection accu v
      ) (Region.tt()) game_list
  in
  product,product_bdd, winning

