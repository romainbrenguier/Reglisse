open Game
open Attractor

type value = Winning | CoopWinning | Help | Losing | NotWinning | NotLosing

(* Values have the following meaning:
 * Wininng -> there is a winning strategy
 * CoopWinning -> there is no winning strategy but we can win if other players cooperate
 * Losing -> it is impossible to win
 * 
 * help -> value is 0 but the adversary can take two different actions which result in a state of value at least 0
 *)

(*let winning p controllables uncontrollables ?(weak=false) unsafe = 
  Region.negation (trap p controllables uncontrollables ~weak unsafe)
 *)

let losing game = 
  let g = { game with contr = (List.rev_append game.contr game.uncontr) } in
  trap g
  (* p controllables uncontrollables unsafe = 
  trap p (List.rev_append controllables uncontrollables) [] unsafe*)

let value ?(weak=false) g = 
  let winning_region = safe ~weak g in
  (*winning g.circuit g.contr g.uncontr ~weak g.err in*)
  let losing_region = losing g in 
  (*g.circuit g.contr g.uncontr g.err in*)
  let not_winning_region = Region.negation winning_region in
  let not_losing_region = Region.negation losing_region in
  let coop_winning_region = Region.intersection not_winning_region not_losing_region in
  function 
  | Winning -> winning_region
  | Losing -> losing_region
  | CoopWinning -> coop_winning_region
  | NotWinning -> not_winning_region
  | NotLosing -> not_losing_region
  | Help -> failwith "Help states not implemented"     


let strategies aig_bdd v =
  let stratW = Strategy.of_region aig_bdd (v Winning) in
  (*let stratC = Strategy.of_region aig_bdd (v CoopWinning) in
  let stratL = Strategy.of_region aig_bdd (v Losing) in*)
  (*Strategy.disj [stratW;stratC;stratL]; print_endline "this is not correct"; *)
  print_endline "Warning: Admissibilty.strategies does not compute all admissible strategies"; 
  Strategy.disj [stratW]
  

let admissible_strategies game = 
  (*let (a,g,c,u,err) = game in*)
  let v = value game in (* g c u err in*)
  (if Region.includes_initial (v Winning) then 1
   else if Region.includes_initial (v Losing) then -1
   else 0), strategies game.circuit v

let compositional_synthesis game_list =
  let admissibles = List.map snd (List.map admissible_strategies game_list) in
  
  let conjunction = Strategy.conj admissibles in
  Timer.log "computation for subgames finished";

  let product = 
    List.fold_left 
      (fun accu g -> Aiger.compose accu g.aiger) Aiger.empty game_list
  in
  Timer.log "aiger product finished";

  let product_bdd = AigerBdd.Circuit.of_aiger product in
  Timer.log "circuit of the product finished";

  let add_list set list = List.fold_left (fun a b -> AigerBdd.VariableSet.add b a) set list in
  let of_list = add_list AigerBdd.VariableSet.empty in

  let all_inputs = 
    List.fold_left
      (fun accu g -> add_list (add_list accu g.contr) g.uncontr
      ) AigerBdd.VariableSet.empty game_list
  in

  let controllable_inputs =
    List.fold_left
      (fun accu g -> add_list accu g.contr) AigerBdd.VariableSet.empty game_list
  in


  let _,winning =
    List.fold_left
      (fun (i,accu) game ->
       let u = AigerBdd.VariableSet.elements (AigerBdd.VariableSet.diff all_inputs (of_list game.contr)) in
       let restriction = 
	 (*List.fold_left
	   (fun (j,accu) adm -> 
	    if j = i then j+1,accu else j+1, Cudd.bddAnd adm accu
	   ) (0,Cudd.bddTrue()) (List.rev admissibles)*)
	 Strategy.conj admissibles
       in
       let unsafe = Region.of_bdds game.err (Cudd.bddFalse()) in
       failwith "in Admissibility.compositional_synthesis"
       (*let v = Region.negation (Attractor.trap_with_restriction product_bdd c u unsafe restriction) in
       i+1, Region.intersection accu v*)
      ) (0,Region.tt()) game_list
  in
  Timer.log "winning strategy in the product finished";

  product,product_bdd,controllable_inputs,winning 
