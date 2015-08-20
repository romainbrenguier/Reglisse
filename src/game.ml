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

let of_aiger aig inputs outputs errors = 
  let aigerBdd = AigerBdd.Circuit.of_aiger aig in
  let unsafe = AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol (errors,Some 0))) in 
  let contr,uncontr = control aig inputs outputs in 
  let game = {aiger=aig; circuit=aigerBdd; contr=contr; uncontr=uncontr; err=unsafe} in
  game
