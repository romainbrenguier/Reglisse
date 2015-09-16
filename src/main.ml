type synthesis_method = Classical | Cooperative | Adversarial | Admissible

let output_game = ref true
let output_product = ref true
let synthesis_method = ref Classical

let arguments = 
  let open Arg in
  [ "-g", Set output_game, "Output the generated games in an aiger file";
    "-p", Set output_product, "Output the global product in an aiger file";
    "-l", Set Timer.display_log, "Display logs";
    "-d", Set Common.display_debug, "Display debug informations";
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
  Timer.log ("writing aiger in "^output_file);
  let outch = open_out output_file in
  Aiger.write aig outch;
  close_out outch

let write_verilog file module_name aig =
  let output_file = file^".v" in
  Timer.log ("writing verilog in "^output_file);
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

let classical_synthesis modules =
  Timer.log "Classical synthesis";
  let open Reglisse in
  let env = Reglisse.Env.create (List.length modules) in
  let rec aux m =
    Timer.log ("Module "^m.module_name);
    Reglisse.Env.new_module env m.module_name m.inputs m.outputs;
    match Reglisse.safety_to_game m with 
    | Some game -> 
       Timer.log "Atomic module";
       Reglisse.Env.add_game env m.module_name game
    | None -> 
       Timer.log "Composition module";
       match Reglisse.calls_to_game env m with 
       | None -> failwith "in Main.classical_synthesis: Reglisse.calls_to_game failed"
       | Some game ->
	  let open Game in
	  Timer.log "Product computed";
	  if !output_product then write_aiger "product" game.Game.aiger;
	  let winning = Attractor.safe game in
	  if Region.includes_initial winning 
	  then Timer.log "realizable"
	  else (Timer.log "unrealizable"; raise Unrealizable);
	  let strat = Strategy.of_region game.circuit winning in
	  let aiger_strat = Strategy.to_aiger game.aiger strat game.contr game.uncontr in
	  Env.add_aiger env m.module_name aiger_strat
  in 
  List.iter aux modules;
  match Reglisse.Env.find_aiger env "Main"
  with Some x -> x 
     | None -> failwith "Module Main not found"


let cooperative_synthesis modules =
  let open Reglisse in
  let env = Reglisse.Env.create (List.length modules) in
  let _,strats = 
    List.fold_left
      (fun (i,strats) modul -> 
       Timer.log ("module "^modul.Reglisse.module_name);
       (* be carreful here "never" gets an integer added *)
       let prefix = "never_"^string_of_int i in
       match Reglisse.safety_to_aiger ~prefix modul with
       | None -> 
	  prerr_endline "Warning: compositional module"; 
	  (*let Some aig = Reglisse.calls_to_game env modul in
	let game = Game.of_aiger aig modul.inputs modul.outputs (prefix^"_accept") in
	let winning = Region.negation (Attractor.trap_with_restriction game strat Admissibility.CoopWinning) in
	if Region.includes_initial winning
	then Timer.log "realizable"
	else failwith "unrealizable";
	let strat = Strategy.of_region game.circuit winning in
	let aiger_strat = Strategy.to_aiger game.aiger strat game.contr game.uncontr in*)
	  ( match Reglisse.calls_to_aiger ~env modul with 
	    | Some aig ->
	       Reglisse.Env.add_aiger env modul.module_name aig;
	       (* Reglisse.add_game env modul.module_name game;*)
	       write_aiger modul.module_name aig; 
	       write_verilog modul.module_name modul.module_name aig;
	       prerr_endline "we should update the strategies";
	       i+1,strats
	    | None -> failwith "cannot convert calls to aiger"
	  )
	    
     | Some aiger ->
	let game = Game.of_aiger aiger modul.inputs modul.outputs (prefix^"_accept") in
	let coop = Admissibility.value game Admissibility.CoopWinning in
	if Region.includes_initial coop
	then Timer.log "coop realizable"
	else failwith "unrealizable";
	Reglisse.Env.add_game env modul.module_name game;
	let new_strat = Strategy.of_region game.Game.circuit coop in
	i+1, new_strat :: strats
      ) (0,[]) modules
  in 
  (* Strategy.conj strats;*)
  match Reglisse.Env.find_aiger env "Main"
  with Some x -> x | None -> failwith "Module Main not found"


let adversarial_synthesis modules =
  Timer.log "Adversarial synthesis";
  let open Reglisse in
  let env = Reglisse.Env.create (List.length modules) in
  let rec aux m =
    Timer.log ("Module "^m.module_name);
    Reglisse.Env.new_module env m.module_name m.inputs m.outputs;
    match Reglisse.safety_to_game m with 
    | Some game -> 
       let open Game in
       Timer.log "Atomic module";
       let winning = Attractor.safe game in
       if Region.includes_initial winning 
       then Timer.log "realizable"
       else (Timer.log "unrealizable"; raise Unrealizable);
       let strat = Strategy.of_region game.circuit winning in
       let aiger_strat = Strategy.to_aiger game.aiger strat game.contr game.uncontr in
       Env.add_aiger env m.module_name aiger_strat

    | None -> 
       Timer.log "Composition module";
       match Reglisse.calls_to_aiger ~env m with 
       | None -> failwith "in Main.adversarial_synthesis: Reglisse.calls_to_aiger failed"
       | Some aiger -> Env.add_aiger env m.module_name aiger
  in 
  List.iter aux modules;
  match Reglisse.Env.find_aiger env "Main"
  with Some x -> x 
     | None -> failwith "Module Main not found"



let admissible_synthesis modules =
  let env = Reglisse.Env.create (List.length modules) in
  
  print_endline "assume admissible synthesis";
  let aig = Reglisse.Env.find_game_exn env "Main" in
  let product_aig,product,controllables,winning = 
    Admissibility.compositional_synthesis [aig]
  in
  
  if Region.includes_initial winning
  then print_endline "realizable"
  else failwith "unrealizable";

  failwith "Main.admissible_synthesis not implemented"

  (*
  let value,adm = Admissibility.admissible_strategies game in
  Hashtbl.add tab_adm modul.module_name adm;
  Timer.log ("Value for module "^modul.module_name^" = "^string_of_int value);
  if value = 1 
  then 
    let aig_strategy = Strategy.to_aiger aiger adm game.Game.contr game.Game.uncontr in
    let aig_strategy = Aiger.hide aig_strategy (prefix^"_accept",Some 0) in
    Reglisse.Env.new_module env modul.module_name modul.inputs modul.outputs;
    Reglisse.Env.add_aiger env modul.module_name aig_strategy;
    write_aiger modul.module_name aig_strategy; 
    write_verilog modul.module_name modul.module_name aig_strategy		       
  else ();

  Timer.log "computing strategy";
  let strategy = Strategy.of_region product winning in
  let contr = List.fold_left (fun a b -> AigerBdd.VariableSet.add b a) AigerBdd.VariableSet.empty aig.contr in
  let uncontr = List.fold_left (fun a b -> AigerBdd.VariableSet.add b a) AigerBdd.VariableSet.empty aig.uncontr in
  let aig_strategy = Strategy.to_aiger product_aig strategy (AigerBdd.VariableSet.elements contr) (AigerBdd.VariableSet.elements uncontr) in
  Timer.log "assume_admissible_synthesis finished";
  aig_strategy*)



    


let main file = 
  let specs = Reglisse.parse_file file in
  Cudd.init 100;
  Timer.log ("Parsed file "^file);

  let aig = match !synthesis_method with 
    | Classical -> classical_synthesis specs 
    | Cooperative -> cooperative_synthesis specs
    | Adversarial -> adversarial_synthesis specs 
    | Admissible -> admissible_synthesis specs 
  in
  Timer.log "exporting to aiger";
  write_aiger file aig;
  Timer.log "exporting to verilog";
  write_verilog file "Main" aig


  
let parse_arguments =
  Arg.parse arguments main "usage: ./reglisse <options> spec.rgl"

  
    

  
