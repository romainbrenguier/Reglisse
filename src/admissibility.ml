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
  (*print_endline "controllables:";
  List.iter print_endline (List.map AigerBdd.Variable.to_string controllables);
  print_endline "uncontrollables:";
  List.iter print_endline (List.map AigerBdd.Variable.to_string uncontrollables);*)
  let winning_region = winning p controllables uncontrollables ~weak unsafe in
  (*print_endline "writing winning1.dot";
  Cudd.dumpDot "winning1.dot" (Region.latch_configuration winning_region);*)
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


let strategies aig_bdd v =
  let not_winning = Region.latch_configuration (v NotWinning) in
  let successor_not_winning = 
    Cudd.bddVectorCompose (AigerBdd.Circuit.rename_configuration not_winning (AigerBdd.Circuit.array_variables aig_bdd) (AigerBdd.Circuit.array_next_variables aig_bdd)) (AigerBdd.Circuit.composition_vector aig_bdd) 
  in
  let losing = Region.latch_configuration (v Losing) in
  let successor_losing = Cudd.bddVectorCompose (AigerBdd.Circuit.rename_configuration losing (AigerBdd.Circuit.array_variables aig_bdd) (AigerBdd.Circuit.array_next_variables aig_bdd)) (AigerBdd.Circuit.composition_vector aig_bdd) in
  Region.union (v Losing) (Region.union (v Winning) (Region.intersection_bdd (v Winning) (Cudd.bddNot successor_losing)))
  (*Region.intersection 
    (Region.negation (Region.intersection_bdd (v Winning) successor_not_winning))
    (Region.negation (Region.intersection_bdd (v NotLosing) successor_losing))
   *)

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

type game = Aiger.t * AigerBdd.Circuit.t * AigerBdd.Variable.t list * AigerBdd.Variable.t list * Cudd.bdd

let admissible_strategies game = 
  let (a,g,c,u,err) = game in
  let v = value g c u err in
  (if Region.includes_initial (v Winning) then 1
   else if Region.includes_initial (v Losing) then -1
   else 0), strategies g v

let compositional_synthesis game_list =

  let admissibles = List.map snd (List.map admissible_strategies game_list) in

  let conjunction = 
    List.fold_left Region.intersection (Region.tt()) admissibles
  in

  Timer.log "computation for subgames finished";

  let product = 
    List.fold_left 
      (fun accu (a,_,_,_,_) -> 
	Aiger.compose accu a
      ) Aiger.empty game_list
  in
  Timer.log "aiger product finished";

  let product_bdd = AigerBdd.Circuit.of_aiger product in
  Timer.log "circuit of the product finished";

  let add_list set list = List.fold_left (fun a b -> AigerBdd.VariableSet.add b a) set list in
  let of_list = add_list AigerBdd.VariableSet.empty in

  let all_inputs = 
    List.fold_left
      (fun accu (_,_,c,u,_) -> 
       add_list (add_list accu c) u
      ) AigerBdd.VariableSet.empty game_list
  in

  let controllable_inputs =     
    List.fold_left
      (fun accu (_,_,c,u,_) -> add_list accu c
      ) AigerBdd.VariableSet.empty game_list
  in


  let _,winning =
    List.fold_left
      (fun (i,accu) (a,g,c,_,err) ->
       let u = AigerBdd.VariableSet.elements (AigerBdd.VariableSet.diff all_inputs (of_list c)) in
       let _,restriction = 
	 List.fold_left
	   (fun (j,accu) adm -> if j = i then j+1,accu else j+1, Region.intersection adm accu
	   ) (0,Region.tt()) (List.rev admissibles)
       in
       let unsafe = Region.of_bdds err (Cudd.bddFalse()) in
       let v = Region.negation (Attractor.trap_with_restriction product_bdd c u unsafe restriction) in
       i+1, Region.intersection accu v
      ) (0,Region.tt()) game_list
  in
  Timer.log "winning strategy in the product finished";

  product,product_bdd,controllable_inputs,winning 
