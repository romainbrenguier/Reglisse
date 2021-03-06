type synthesis_method = Classical | Cooperative | Adversarial | Admissible

let output_game = ref true
let output_product = ref true
let display_total_time = ref false
let synthesis_method = ref Classical
let input_file = ref ""

let set_input_file f = input_file := f

let arguments = 
  let open Arg in
  [ "--output-games", Set output_game, "Output the generated games in an aiger file";
    "--output-product", Set output_product, "Output the global product in an aiger file";
    "-l", Set Message.display_log, "Display logs";
    "-t", Set display_total_time, "Display total time";
    "-w", Set Message.display_warning, "Display warnings";
    "-d", Set ReglisseCommon.display_debug, "Display debug informations";
    "-m", Int (function  
	       | 0 -> synthesis_method := Classical
	       | 1 -> synthesis_method := Cooperative
	       | 2 -> synthesis_method := Adversarial
	       | 3 -> synthesis_method := Admissible
	       | _ -> failwith "in command line arguments: no synthesis method corresponding to this number"
	     )
      , "Set the synthesis method: 0 Classical | 1 Cooperative | 2 Adversarial | 3 Admissible"
  ]

    
let write_aiger name aig =
  let output_file = name^".aag" in
  Message.log ("writing aiger in "^output_file);
  let outch = open_out output_file in
  AigerImperative.write outch aig;
  close_out outch

let write_verilog file module_name aig =
  let output_file = file^".v" in
  Message.log ("writing verilog in "^output_file);
  let outch = open_out output_file in
  Verilog.of_aiger module_name aig outch;
  close_out outch

(*
let merge_outputs aiger = 
  match aiger.Aiger.outputs with 
  | [] -> failwith "in Main.merge_outputs: no outputs"
  | [hd] -> aiger
  | hd :: tl ->
     let aig = List.fold_left (fun aig x -> Aiger.hide aig (Aiger.lit2symbol aig x)) aiger (hd :: tl) in
     let aig, no_error =
       List.fold_left 
	 (fun (aig,gate) out -> 
	  let aig, v = Aiger.new_var aig in
	  let aig = Aiger.add_and aig (Aiger.var2lit v) gate (Aiger.aiger_not out) in
	  aig, (Aiger.var2lit v)
	 ) (aig,Aiger.aiger_not hd) tl
     in 
     Aiger.add_output aig (Aiger.aiger_not no_error) ("error",Some 0) 

let make_controllable aiger controllables =
  List.fold_left 
    (fun aig input -> 
     if AigerBdd.VariableSet.mem (AigerBdd.Variable.of_lit aiger input) controllables
     then 
       let syms,symo = Aiger.lit2symbol aiger input in
       Aiger.rename aig [(syms,symo),("controllable_"^syms,symo)]
     else aig
    ) aiger aiger.Aiger.inputs
  *)

exception Unrealizable
exception NoMainModule

(* compute an aiger representation of a winning strategy in the game *)
let general_synthesis game =
  let open Game in
  Message.log "General synthesis";
  if !ReglisseCommon.display_debug then 
    (print_endline "Controllables:\n";
     List.iter (fun x -> print_endline (BddVariable.to_string x)) game.contr;
     print_endline "Uncontrollables:\n";
     List.iter (fun x -> print_endline (BddVariable.to_string x)) game.uncontr;
     print_endline "Game:\n";
     AigerImperative.write stdout game.aiger
    );
  let winning = Attractor.safe game in
  if Region.includes_initial winning 
  then Message.log "realizable"
  else (Message.log "unrealizable"; raise Unrealizable);
  let strat = Strategy.of_region game.circuit winning in
  let aiger_strat = Strategy.to_aiger game.aiger strat game.contr game.uncontr in
  if !ReglisseCommon.display_debug then 
    (print_endline "Strategy as aiger file:";
     AigerImperative.write stdout aiger_strat
    );
  aiger_strat

  
let classical_synthesis modules =
  Message.log "Classical synthesis";
  let open Reglisse in
  let nb_modules = List.length modules in
  let env = Reglisse.Env.create nb_modules in
  let module_tab = Hashtbl.create nb_modules in
  let rec aux m =
    Message.log ("Module "^m.module_name);
    Reglisse.Env.new_module env m.module_name m.inputs m.outputs;
    if Reglisse.is_atomic m 
    then
      (Message.log "Atomic module";
       (* Reglisse.Env.add_game env m.module_name game;*)
       Hashtbl.add module_tab m.module_name m)
    else
      (
	Message.log "Composition module";
	match Reglisse.calls_to_game env module_tab m with 
	| None -> failwith "in Main.classical_synthesis: Reglisse.calls_to_game failed"
	| Some (game,_) ->
	  Message.log "Product computed";
	  if !output_product then write_aiger "product" game.Game.aiger;
	  let aiger_strat = general_synthesis game in
	  Env.add_aiger env m.module_name aiger_strat
      )

(*
  match Reglisse.safety_to_game m with 
    | Some game -> 
      | None -> 
*)
  in match modules with 
  | [m] ->   
     (
       Message.log "Single module. Generating safety game";
       match Reglisse.safety_to_game m with
       | Some game -> general_synthesis game
       | None -> failwith "in Main.classical_synthesis: Reglisse.safety_to_game failed"
    )
  | _ ->
    List.iter aux modules;
    match Reglisse.Env.find_aiger env "Main"
    with Some x -> x | None -> 
      Message.warning "No Main module. If there are more than one module, one of them must be named Main.";
      raise NoMainModule


let cooperative_synthesis modules =
  Message.log "Cooperative synthesis";
  let open Reglisse in
  let nb_modules = List.length modules in
  let env = Reglisse.Env.create nb_modules in
  let module_tab = Hashtbl.create nb_modules in
  (* records cooperative strategies for each module *)
  let strats = Hashtbl.create nb_modules in
  let aux m = 
    Message.log ("Module "^m.module_name);
    Reglisse.Env.new_module env m.module_name m.inputs m.outputs;
    match Reglisse.safety_to_game m with 
    | Some game -> 
       Message.log "Atomic module";
       let coop = Region.negation (Admissibility.losing game) in
       if Region.includes_initial coop
       then Message.log "coop realizable"
       else (Message.log "unrealizable"; raise Unrealizable);
       Reglisse.Env.add_game env m.module_name game;
       let new_strat = Strategy.of_region game.Game.circuit 
	 (* Why do we need to take this ? *)
	 (Region.union (Region.of_bdds (Cudd.bddTrue()) (Cudd.bddFalse())) coop) 
       in
       Hashtbl.add module_tab m.module_name m;
       Hashtbl.add strats m.module_name new_strat

    | None -> 
       Message.log "Composition module";
       match Reglisse.calls_to_game env module_tab m with 
       | None -> failwith "in Main.classical_synthesis: Reglisse.calls_to_game failed"
       | Some (game,call_renaming_list) ->
	  let open Game in
	  Message.log "Product computed";
	  if !output_product then write_aiger "product" game.Game.aiger;
	  Message.log "Computing winning region with some restriction";
	  let strategies = call_renaming_list |> 
	      List.map (fun {call;renaming} -> 
	      let strat = 
		try Hashtbl.find strats call 
		with Not_found -> Message.warning ("No strategy found for module "^call); 
		  Strategy.all ()
	      in Strategy.rename strat renaming 
	      )
	  in
	  let strategy = Strategy.conj strategies in
	  let winning = Region.negation (Attractor.trap ~strategy game) in
	  if Region.includes_initial winning 
	  then Message.log "realizable"
	  else (Message.log "unrealizable"; raise Unrealizable);
	  let strat = Strategy.of_region game.circuit winning in
	  let aiger_strat = Strategy.to_aiger game.aiger strat game.contr game.uncontr in
	  Env.add_aiger env m.module_name aiger_strat
  in 
  match modules with 
  | [m] ->
    (
      match Reglisse.safety_to_game m with
      | Some game -> general_synthesis game
      | None -> failwith "in Main.cooperative_synthesis: Reglisse.safety_to_game failed"
    )

  | _ ->
    List.iter aux modules;
    match Reglisse.Env.find_aiger env "Main"
    with Some x -> x | None -> raise NoMainModule


let adversarial_synthesis modules =
  Message.log "Adversarial synthesis";
  let open Reglisse in
  let env = Reglisse.Env.create (List.length modules) in
  let rec aux m =
    Message.log ("Module "^m.module_name);
    Reglisse.Env.new_module env m.module_name m.inputs m.outputs;
    match m.content with 
    | Safety _ -> 
      ( match Reglisse.safety_to_game m with 
      | Some game -> 
	let open Game in
	Message.log "Atomic module";
	let winning = Attractor.safe game in
	if Region.includes_initial winning 
	then Message.log "realizable"
	else (Message.log "unrealizable"; raise Unrealizable);
	let strat = Strategy.of_region game.circuit winning in
	let aiger_strat = Strategy.to_aiger game.aiger strat game.contr game.uncontr in
	Env.add_aiger env m.module_name aiger_strat
      | None -> failwith "in Main.adversarial_synthesis: Reglisse.safety_to_game failed"
      )
    | Calls _ -> 
      ( Message.log "Composition module";
	match Reglisse.calls_to_aiger ~env m with 
	| Some aiger -> Env.add_aiger env m.module_name aiger
	| None -> failwith "in Main.adversarial_synthesis: Reglisse.calls_to_aiger failed")
    | Functional _ ->
      ( Message.log "Functional module";
	Message.warning "in the composition of functional module, the same variable used in different modules may be considered as one variable";
	match Reglisse.functional_to_aiger m with 
	| Some aiger -> Env.add_aiger env m.module_name aiger
	| None -> failwith "in Main.adversarial_synthesis: Reglisse.functional_to_aiger failed")    

  in 
  match modules with 
  | [m] ->
    (
      match Reglisse.safety_to_game m with
      | Some game -> general_synthesis game
      | None -> match Reglisse.functional_to_aiger m with 
	| Some aiger -> aiger
	| None -> failwith "in Main.adversarial_synthesis: Reglisse.safety_to_game and Reglisse.functional_to_aiger failed"
    )

  | _ ->
    List.iter aux modules;
    match Reglisse.Env.find_aiger env "Main"
    with Some x -> x | None -> raise NoMainModule


let admissible_synthesis modules =
  Message.log "Assume admissible synthesis";
  let open Reglisse in
  let nb_modules = List.length modules in
  let env = Reglisse.Env.create nb_modules in
  let module_tab = Hashtbl.create nb_modules in
  (* records admissible strategies for each module *)
  let strats = Hashtbl.create nb_modules in
  let aux m = 
    Message.log ("Module "^m.module_name);
    Reglisse.Env.new_module env m.module_name m.inputs m.outputs;
    match Reglisse.safety_to_game m with 
    | Some game -> 
       Message.log "Atomic module";
       let value,adm = Admissibility.admissible_strategies game in
       (match value with
       | 0 -> Message.log "Cooperatively realizable"
       | 1 -> Message.log "Adversarialy realizable"
       | _ -> Message.log "unrealizable"; raise Unrealizable);
       Reglisse.Env.add_game env m.module_name game;
       (*let new_strat = Strategy.of_region game.Game.circuit 
	 (* Why do we need to take this ? *)
	 (Region.union (Region.of_bdds (Cudd.bddTrue()) (Cudd.bddFalse())) coop) 
       in*)
       Hashtbl.add module_tab m.module_name m;
       Hashtbl.add strats m.module_name adm

    | None -> 
       Message.log "Composition module";
       match Reglisse.calls_to_game env module_tab m with 
       | None -> failwith "in Main.classical_synthesis: Reglisse.calls_to_game failed"
       | Some (game,call_renaming_list) ->
	  let open Game in
	  Message.log "Product computed";
	  if !output_product then write_aiger "product" game.Game.aiger;
	  Message.log "Computing winning region with some restriction";
	  let strategies = call_renaming_list |> 
	      List.map (fun {call;renaming} -> 
	      let strat = 
		try Hashtbl.find strats call 
		with Not_found -> Message.warning ("No strategy found for module "^call); 
		  Strategy.all ()
	      in Strategy.rename strat renaming 
	      )
	  in
	  let strategy = Strategy.conj strategies in
	  let winning = Region.negation (Attractor.trap ~strategy game) in
	  if Region.includes_initial winning 
	  then Message.log "realizable"
	  else (Message.log "unrealizable"; raise Unrealizable);
	  let strat = Strategy.of_region game.circuit winning in
	  let aiger_strat = Strategy.to_aiger game.aiger strat game.contr game.uncontr in
	  Env.add_aiger env m.module_name aiger_strat
  in 
  match modules with 
  | [m] ->
    (
      match Reglisse.safety_to_game m with
      | Some game -> general_synthesis game
      | None -> failwith "in Main.admissible_synthesis: Reglisse.safety_to_game failed"
    )

  | _ ->
    List.iter aux modules;
    match Reglisse.Env.find_aiger env "Main"
    with Some x -> x | None -> raise NoMainModule


let parse_arguments =
  Arg.parse arguments set_input_file "usage: ./reglisse <options> spec.rgl"

let main = 
  let specs = Reglisse.parse_file !input_file in
  Cudd.init 100;
  Message.log ("Parsed file "^ !input_file);

  let aig = match !synthesis_method with 
    | Classical -> classical_synthesis specs 
    | Cooperative -> cooperative_synthesis specs
    | Adversarial -> adversarial_synthesis specs 
    | Admissible -> admissible_synthesis specs 
  in
  Message.log "exporting to aiger";
  write_aiger !input_file aig;
  Message.log "exporting to verilog";
  write_verilog !input_file "Main" aig;
  if !display_total_time then (Message.display (); print_newline ())


  
