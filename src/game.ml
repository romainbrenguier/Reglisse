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

type t = {
  aiger:Aiger.t; 
  circuit: AigerBdd.Circuit.t; 
  contr: AigerBdd.Variable.t list;
  uncontr: AigerBdd.Variable.t list;
  err: Cudd.bdd
}

let control aiger inputs outputs = 
  let contr =
    List.fold_left 
      (fun accu o -> 
	try Aiger.symbol2lit aiger (o,Some 0) :: accu
	with Not_found -> Printf.eprintf "warning in Reglisse.synthesis: variable %s does not appear in the automaton\n" o; accu
      ) [] outputs
  in
  let uncontr = 
    List.fold_left 
      (fun accu o -> 
	try Aiger.symbol2lit aiger (o,Some 0) :: accu
	with Not_found -> Printf.eprintf "warning in Reglisse.synthesis: variable %s does not appear in the automaton\n" o; accu
      )[] inputs
  in
  List.map (AigerBdd.Variable.of_lit aiger) contr,
  List.map (AigerBdd.Variable.of_lit aiger) uncontr

let initial_state g = 
  Region.initial_state g.aiger

let of_aiger ~aig ~inputs ~outputs ~errors = 
  let aigerBdd = AigerBdd.Circuit.of_aiger aig in
  let error_bdds =
    List.map
      (fun x -> AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol (x,Some 0))))
      errors
  in 
  let unsafe = List.fold_left Cudd.bddOr (Cudd.bddFalse()) error_bdds in
  let contr,uncontr = control aig inputs outputs in 
  let game = {aiger=aig; circuit=aigerBdd; contr=contr; uncontr=uncontr; err=unsafe} in
  game

let product game_list =
  let product = 
    List.fold_left 
      (fun accu g -> Aiger.compose accu g.aiger) Aiger.empty game_list
  in
  let product = AigerBdd.reorder_aiger product in
  let circuit = AigerBdd.Circuit.of_aiger product in
  let add_list set list = List.fold_left (fun a b -> AigerBdd.VariableSet.add b a) set list in
  let all_inputs = 
    List.fold_left
      (fun accu g -> add_list (add_list accu g.contr) g.uncontr
      ) AigerBdd.VariableSet.empty game_list
  in

  let contr_set =
    List.fold_left
      (fun accu g -> add_list accu g.contr) AigerBdd.VariableSet.empty game_list
  in
  let contr = AigerBdd.VariableSet.elements contr_set in
  let uncontr = AigerBdd.VariableSet.elements (AigerBdd.VariableSet.diff all_inputs contr_set) in
  let err = List.fold_left (fun a g -> Cudd.bddOr a g.err) (Cudd.bddFalse()) game_list in
  {aiger=product; circuit=circuit;contr;uncontr;err}


let rename game renaming = 
  let aiger = Aiger.full_rename game.aiger renaming in
  print_endline "in Game.rename : we could avoid a recomputation of the BDD here";
  let circuit = AigerBdd.Circuit.of_aiger aiger in
  (*let contr = List.map (fun x -> try List.assoc x renaming with Not_found -> x) game.contr in
  let uncontr = List.map (fun x -> try List.assoc x renaming with Not_found -> x) game.uncontr in*)
  let array_renaming = Array.of_list renaming in
  let array1 = 
    Array.init 
      (List.length renaming)
      (fun i -> AigerBdd.Variable.find (fst array_renaming.(i),0))
  in
  let array2 = 
    Array.init 
      (List.length renaming)
      (fun i -> AigerBdd.Variable.find (snd array_renaming.(i),0))
  in
  let err = AigerBdd.Circuit.rename_configuration game.err array1 array2 in
  {aiger;circuit;contr=game.contr;uncontr=game.uncontr;err}
