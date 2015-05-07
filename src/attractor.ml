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
  let renamed = Cudd.bddSwapVariables (Region.latch_configuration unsafe) (AigerBdd.array_variables aiger) (AigerBdd.array_next_variables aiger) in
  let substitued = Cudd.bddVectorCompose renamed (AigerBdd.composition_vector aiger) in
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
  Cudd.bddVectorCompose (Cudd.bddSwapVariables
			   (Region.latch_configuration region)
			   (AigerBdd.array_variables game) 
			   (AigerBdd.array_next_variables game))
			(AigerBdd.composition_vector game)



let join_output_input aiger ~output ~input =
  let map_inputs = Hashtbl.create 100 in
  let map_latches = Hashtbl.create 100 in

  let aig = 
    List.fold_left
      (fun accu i ->
	if i = input then accu
	else
	  let aig,v = Aiger.new_var accu in
	  let lit = Aiger.var2lit v in
	  Hashtbl.add map_inputs i lit;
	  Aiger.add_input aig lit (Aiger.lit2symbol aiger i)
      ) Aiger.empty aiger.Aiger.inputs
  in

  let aig = 
    List.fold_left
      (fun accu (l,_) ->
	let aig,v = Aiger.new_var accu in
	let lit = Aiger.var2lit v in
	Hashtbl.add map_latches l lit;
	aig
      ) aig aiger.Aiger.latches
  in

  let cache = Hashtbl.create 100 in

  (* start by adding gates are necessary to compute the output *)
  let rec aux aig gate =
    (* Printf.printf "aux aig %d\n" (Aiger.lit2int gate);*)
    if Hashtbl.mem cache (Aiger.strip gate)
    then if Aiger.sign gate 
      then aig, Aiger.aiger_not (Hashtbl.find cache (Aiger.strip gate))
      else aig, Hashtbl.find cache gate
    else
      let aig,lit =
	match Aiger.lit2tag aiger (Aiger.strip gate) with
	| Constant true -> aig, Aiger.aiger_true
	| Constant false -> aig, Aiger.aiger_false
	| Input l -> aig, (try Hashtbl.find map_inputs l with
	  Not_found -> failwith "in Reglisse.join_input_output: the output depends on the input")
	| Latch (l,_) -> aig,Hashtbl.find map_latches l
	| And (g,l,r) -> 
	  let aig1,lit1 = aux aig l in
	  let aig2,lit2 = aux aig1 r in
	  let aig3,v = Aiger.new_var aig2 in
	  let lit = Aiger.var2lit v in
	  let aig = Aiger.add_and aig3 lit lit1 lit2 in
	  aig, lit
      in Hashtbl.add cache (Aiger.strip gate) lit;
      if Aiger.sign gate then aig, Aiger.aiger_not lit else aig, lit
  in 

  let aig,lit = aux aig output in

  (* Printf.printf "adding to map_inputs : %d -> %d\n" (Aiger.lit2int input) (Aiger.lit2int lit);*)
  Hashtbl.add map_inputs input lit;
  
  let aig = 
    List.fold_left 
      (fun accu (l,u) ->
	let aig,lit = aux accu u in
	Aiger.add_latch aig (Hashtbl.find map_latches l) lit (Aiger.lit2symbol aiger l)
      ) aig aiger.Aiger.latches 
  in
  let aig = 
    List.fold_left 
      (fun accu o ->
	if o = output then 
	  Aiger.add_output accu lit (Aiger.lit2symbol aiger o)
	else
	  let aig,lit = aux accu o in
	  (*Printf.printf "adding output %s\n" (Aiger.Symbol.to_string (Aiger.lit2symbol aiger o));*)
	  Aiger.add_output aig lit (Aiger.lit2symbol aiger o)
      ) aig aiger.Aiger.outputs
  in

  aig



let strategy_to_aiger aiger strategy controllables uncontrollables= 
  let bdd_of_lit lit = 
    AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol (Aiger.lit2symbol aiger lit)))
  in

  let cube = AigerBdd.Variable.make_cube controllables in

  let var_map = AigerBdd.map_of_aiger aiger in

  (*print_endline "MAP:";
  print_endline (AigerBdd.map_to_string var_map);
  *)

  (*  (* adding variables as input except controllable ones *)
  let circuit = 
    AigerBdd.VariableMap.fold
      (fun v lit c -> 
	if List.mem v controllables then c 
	else
	  let sym = Aiger.lit2symbol aiger lit in
	  let c,var = Aiger.new_var c in
	  Aiger.add_input c (Aiger.var2lit var) sym
      ) var_map_aiger Aiger.empty
  in

  let var_map = AigerBdd.map_of_aiger circuit in
  *)
  let circuit = aiger in

  let rec aux (bdd,circuit) contr = 
    (*Printf.printf "controlling variable: %d\n" (AigerBdd.Variable.to_int contr);*)
    let bdd_inp = AigerBdd.Variable.to_bdd contr in 
    (* val is true when there we can put the variable contr to true *)
    let val_inp = Cudd.bddExistAbstract (Cudd.bddAnd bdd_inp bdd) (AigerBdd.Variable.make_cube [contr]) in

    if !Common.display_debug
    then Cudd.dumpDot ("strategy_output_"^string_of_int (AigerBdd.Variable.to_int contr)^".dot") val_inp;

    let circuit, output = AigerBdd.add_bdd_to_aiger circuit val_inp var_map in
    let output_symbol = 
      let (s,i) = 
	Aiger.lit2symbol aiger (AigerBdd.VariableMap.find contr var_map)
      in ("_tmp_"^s,i)
    in
    let circuit = Aiger.add_output circuit output output_symbol in
    (*Cudd.bddAnd bdd (Cudd.bddOr 
		       (Cudd.bddAnd val_inp bdd_inp)
		       (Cudd.bddAnd (Cudd.bddNot val_inp) (Cudd.bddNot bdd_inp))),*)
    
    let bdd =  Cudd.bddExistAbstract 
      (Cudd.bddAnd bdd (Cudd.bddOr 
			  (Cudd.bddAnd val_inp bdd_inp)
			  (Cudd.bddAnd (Cudd.bddNot val_inp) (Cudd.bddNot bdd_inp))))
      (AigerBdd.Variable.make_cube [contr])

    in 
    if !Common.display_debug
    then Cudd.dumpDot ("strategy_rest_"^string_of_int (AigerBdd.Variable.to_int contr)^".dot") bdd;
    bdd, circuit
  in

  if !Common.display_debug then Cudd.dumpDot ("strategy.dot") strategy;

  let bdd,circuit = 
    List.fold_left aux (strategy,circuit) controllables in

  let renaming = 
    List.map 
      (fun contr -> 
	let o = AigerBdd.VariableMap.find contr var_map in
	let (sym_s,sym_i) = Aiger.lit2symbol aiger o in
	("_tmp_"^sym_s,sym_i),(sym_s,sym_i)
      ) controllables
  in

  let aig = 
    List.fold_left 
      (fun accu (output,input) -> 
	(* Printf.printf "joining %s -> %s\n" (Aiger.Symbol.to_string output) (Aiger.Symbol.to_string input);
	Aiger.write accu stdout;*)
	join_output_input accu ~output:(Aiger.symbol2lit accu output) ~input:(Aiger.symbol2lit accu input)
      ) circuit renaming
  in

  Aiger.rename aig renaming
  

let test aiger = 
  let bdd_of_lit lit = 
    AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol (Aiger.lit2symbol aiger lit)))
  in

  let contr,uncontr = controllables aiger in
  let (contrv,uncontrv) = controllable_variables aiger in 
  if !Common.display_debug then List.iter (fun x -> Printf.printf "%d controllable\n" (Aiger.lit2int x)) contr;

  AigerBdd.init aiger;
  let unsafe = List.fold_left Cudd.bddOr (Cudd.bddFalse()) (List.map bdd_of_lit aiger.Aiger.outputs) in
  let initial = AigerBdd.bdd_of_valuation (AigerBdd.initial_state aiger) in
  let precomputation = AigerBdd.of_aiger aiger in   
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
