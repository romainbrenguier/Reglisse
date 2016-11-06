open Game 

let rename_substitute_restrict unsafe aiger =
  let renamed = BddVariable.rename_configuration (Region.latch_configuration unsafe) (Circuit.array_variables aiger) (Circuit.array_next_variables aiger) in
  let substitued = Cudd.bddVectorCompose renamed (Circuit.composition_vector aiger) in
  Cudd.bddOr substitued (Region.latch_input_configuration unsafe)


(** [uncontrollable_predecessors U a x_c x_u strat] computes: 
    \exists x_u.  (\forall x_c. strat => U) 
    It computes the uncontrollable predecessors assuming x_c only plays according to strat which describes a partial strategy.
*)
let uncontrollable_predecessors_with_restriction unsafe aiger controllable_cube uncontrollable_cube weak strat =
  let restricted = rename_substitute_restrict unsafe aiger in
  let implication = 
    (* Cudd.bddOr restricted (Cudd.bddNot (Strategy.to_bdd strat)) in *)
    Cudd.bddRestrict restricted (Strategy.to_bdd strat)  in
  if weak 
  then 
    let exist_quantified = Cudd.bddExistAbstract implication uncontrollable_cube in
    let univ_quantified = Cudd.bddUnivAbstract exist_quantified controllable_cube in
    Region.of_bdds univ_quantified exist_quantified
  else
    let univ_quantified = Cudd.bddUnivAbstract implication controllable_cube in
    let exist_quantified = Cudd.bddExistAbstract univ_quantified uncontrollable_cube in
    Region.of_bdds exist_quantified univ_quantified

let trap_with_restriction aiger controllables uncontrollables ?(weak=false) unsafe restriction =
  let controllable_cube = BddVariable.make_cube controllables in
  let uncontrollable_cube = BddVariable.make_cube uncontrollables in

  let aux u = 
    Timer.debug "in Attractor.trap_with_restriction: uncontrollable_predecessor step";
    uncontrollable_predecessors_with_restriction u aiger controllable_cube uncontrollable_cube weak restriction
  in 
  Region.greatest_fixpoint aux unsafe


let trap ?(weak=false) ?(strategy = (Strategy.conj [])) game =
  trap_with_restriction game.circuit game.contr game.uncontr ~weak (Region.of_bdds game.err (Cudd.bddFalse())) strategy

let safe ?(weak=false) ?(strategy = (Strategy.conj [])) game = Region.negation (trap ~weak ~strategy game)

let attractor_with_restriction aiger controllables uncontrollables ?(weak=false) safe restriction =
  trap_with_restriction aiger uncontrollables controllables ~weak:(not weak) safe restriction

let attractor aiger contr uncontr ?(weak=false) safe = 
  attractor_with_restriction aiger contr uncontr ~weak (Region.of_bdds safe (Cudd.bddFalse())) (Strategy.conj [])


let test aiger = 
  let bdd_of_lit lit = 
    BddVariable.to_bdd (BddVariable.find (AigerImperative.lit2string_exn aiger lit))
  in

  let (contrv,uncontrv) = Game.controllable_variables aiger in 
  if !ReglisseCommon.display_debug then List.iter (fun x -> Printf.printf "%d controllable\n" (BddVariable.to_int x)) contrv;

  AigerImpBdd.init aiger;
  let unsafe = List.fold_left Cudd.bddOr (Cudd.bddFalse()) (List.map bdd_of_lit (AigerImperative.LitSet.elements aiger.AigerImperative.outputs)) in
  let initial = AigerImpBdd.bdd_of_valuation (Circuit.initial_state aiger) in
  let circuit = Circuit.of_aiger aiger in   
  let game = { aiger; circuit; contr=contrv; uncontr=uncontrv; err =unsafe} in
  let trap_set = trap ~weak:false game in
  let trap_set_weak = trap ~weak:true game in
  let initial_losing = Cudd.bddRestrict (Region.latch_configuration trap_set) initial in
  let initial_losing_weak = Cudd.bddRestrict (Region.latch_configuration trap_set_weak) initial in

  if !ReglisseCommon.display_debug 
  then Cudd.dumpDot ("trap.dot") (Region.latch_input_configuration trap_set);

  if Cudd.value initial_losing = 1 
  then print_endline "unrealizable"
  else if Cudd.value initial_losing = 0
  then 
    (print_endline "realizable";
     let strat = Strategy.to_aiger aiger (Strategy.of_region game.circuit (Region.negation trap_set)) contrv uncontrv in
     let file_name = Sys.argv.(1)^".controller.aag" in
     let outch = open_out file_name in
     AigerImperative.write outch strat;
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
  (* ReglisseCommon.display_debug := true;*)
  if Filename.check_suffix Sys.argv.(0) "attractor" 
     || Filename.check_suffix Sys.argv.(0) "attractor.byte" 
  then 
    if Array.length Sys.argv < 2 
    then Printf.printf "usage : %s <file>\n" Sys.argv.(0)
    else 
      let aiger = AigerImperative.read_from_file Sys.argv.(1) in
      test aiger
