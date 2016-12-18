module Aiger = AigerImperative

let controllable_name name = 
     if String.length name > 13 then
       String.sub name 0 13 = "controllable_" 
     else false

let controllables aiger = 
  Aiger.LitSet.fold
    (fun ~index ~lit (c,u) ->
      Printf.printf "looking for lit %d\n" lit;
      let name = Aiger.lit2string_exn aiger lit in
      if controllable_name name then (lit :: c, u) 
      else (c, lit :: u)
    ) ( aiger.Aiger.inputs) ([],[]) 

let controllable_variables aiger =
  let aux = List.map (BddVariable.of_lit_exn aiger) in
  let contr,uncontr = controllables aiger in
  aux contr, aux uncontr

let of_aiger aiger = 
  let c,u = controllables aiger in
  (aiger, c, u)
    
let read_from_file file = 
  let aiger = Aiger.read_from_file_exn file in
  of_aiger aiger

type t = {
  aiger:Aiger.t; 
  circuit: Circuit.t; 
  contr: BddVariable.t list;
  uncontr: BddVariable.t list;
  err: Cudd.bdd
}

let control aiger inputs outputs = 
  let contr =
    List.fold_left 
      (fun accu o -> 
	try Aiger.string2lit_exn aiger o :: accu
	with Aiger.Correspondance_not_found _ -> Printf.eprintf "Warning in Game.control: variable %s does not appear in the automaton\nshould have been controllable\n" o; accu
      ) [] outputs
  in
  let uncontr = 
    List.fold_left 
      (fun accu o -> 
	try Aiger.string2lit_exn aiger o :: accu
	with Aiger.Correspondance_not_found _ -> Printf.eprintf "Warning in Game.control: variable %s does not appear in the automaton should have been uncontrollable\n" o; accu
      )[] inputs
  in
  List.map (BddVariable.of_lit_exn aiger) contr,
  List.map (BddVariable.of_lit_exn aiger) uncontr

let initial_state g = 
  Region.initial_state g.aiger

let of_aiger ~aig ~inputs ~outputs ~errors =
  Message.log "Game.of_aiger : importing AIG";
  let circuit = Circuit.of_aiger aig in
  Message.log "Game.of_aiger : AIG imported";
  let error_bdds =
    List.map
      (fun x -> BddVariable.to_bdd (BddVariable.find x))
      errors
  in 
  let err = List.fold_left Cudd.bddOr (Cudd.bddFalse()) error_bdds in
  let contr,uncontr = control aig inputs outputs in 
  let game = {aiger=aig; circuit; contr; uncontr; err} in
  Message.log "Game generator from AIG";
  game

let product game_list =
  let product = 
    List.fold_left 
      (fun accu g -> Aiger.compose accu g.aiger) ((List.hd game_list).aiger) (List.tl game_list)
  in
  (* let product = AigerBdd.reorder_aiger product in*)
  let circuit = Circuit.of_aiger product in
  let add_list set list = List.fold_left (fun a b -> Circuit.VariableSet.add b a) set list in
  let all_inputs = 
    List.fold_left
      (fun accu g -> add_list (add_list accu g.contr) g.uncontr
      ) Circuit.VariableSet.empty game_list
  in

  let contr_set =
    List.fold_left
      (fun accu g -> add_list accu g.contr) Circuit.VariableSet.empty game_list
  in
  let contr = Circuit.VariableSet.elements contr_set in
  let uncontr = Circuit.VariableSet.elements (Circuit.VariableSet.diff all_inputs contr_set) in
  let err = List.fold_left (fun a g -> Cudd.bddOr a g.err) (Cudd.bddFalse()) game_list in
  {aiger=product; circuit=circuit;contr;uncontr;err}


let rename game renaming = 
  (*let aiger = Aiger.rename game.aiger renaming in
    print_endline "in Game.rename : we could avoid a recomputation of the BDD here";*)
  Aiger.rename game.aiger 
    (fun x -> try List.assoc x renaming with Not_found -> x);
  let circuit = Circuit.of_aiger game.aiger in
  (*let contr = List.map (fun x -> try List.assoc x renaming with Not_found -> x) game.contr in
  let uncontr = List.map (fun x -> try List.assoc x renaming with Not_found -> x) game.uncontr in*)
  let array_renaming = Array.of_list renaming in
  let array1 = 
    Array.init 
      (List.length renaming)
      (fun i -> BddVariable.find (fst array_renaming.(i)))
  in
  let array2 = 
    Array.init 
      (List.length renaming)
      (fun i -> BddVariable.find (snd array_renaming.(i)))
  in
  let err = BddVariable.rename_configuration game.err array1 array2 in
  {aiger=game.aiger;circuit;contr=game.contr;uncontr=game.uncontr;err}
