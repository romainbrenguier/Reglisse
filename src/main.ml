let output_game = ref true
let output_product = ref true

type synthesis_method = Classical | Cooperative | Adversarial | Admissible

let synthesis_method = ref Classical

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

exception NoMainModule 


let classical_synthesis modules =
  let env = Reglisse.Env.create (List.length modules) in
  let rec aux m =
    if m.Reglisse.module_name = "Main"
    then 
      (
	Timer.log "Composing in the Main module";
	match Reglisse.calls_to_game env m with 
	| None -> failwith "Main is not a compositional module"
	| Some game ->
	   Timer.log "Product computed";
	   if !output_product
	   then (
	     Timer.log "writing product.aag";
	     prerr_endline "Warning: not sure what we write here";
	     (* let aig = merge_outputs product.aiger in
    let aig = make_controllable product.aiger product.contr in *)
	     Aiger.write_to_file game.Game.aiger "product.aag";
	   );
	   let winning = Attractor.safe game in
	   if Region.includes_initial winning 
	   then Timer.log "realizable"
	   else failwith "unrealizable";
	   let strat = Strategy.of_region game.circuit winning in
	   let aiger_strat = Strategy.to_aiger game.aiger strat game.contr game.uncontr in
	   Reglisse.Env.add_aiger env m.module_name aiger_strat
      )
    else
      (
	Timer.log ("Module "^m.module_name);
	match Reglisse.safety_to_game m with 
	| None -> failwith "no safety"
	| Some game -> Reglisse.Env.add_game env m.module_name game
      )
  in 
  List.iter aux modules;
  match Reglisse.Env.find_aiger env "Main"
  with Some x -> x 
     | None -> failwith "Module Main not found"


let cooperative_synthesis_exn games = 
  let env = Reglisse.Env.create (List.length games) in
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
	let new_strat = Strategy.of_region game.circuit coop in
	i+1, new_strat :: strats
      ) (0,[]) games
  in 
  (* Strategy.conj strats;*)
  match Reglisse.Env.find_aiger env "Main"
  with Some x -> x | None -> failwith "Module Main not found"


let admissible_synthesis games =
  let env = Reglisse.Env.create (List.length games) in
  
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


    
let adversarial_synthesis games =
  failwith "Main.adversarial_synthesis: not implemented"


let main = 
  if Array.length Sys.argv < 2 then Printf.printf "usage: %s <file.rgl>\n" Sys.argv.(0);
  let specs = Reglisse.parse_file Sys.argv.(1) in
  Cudd.init 100;
  Timer.log ("Parsed file "^Sys.argv.(1));

  let aig = match !synthesis_method with 
    | Classical -> classical_synthesis specs 
    | Cooperative -> cooperative_synthesis_exn specs
    | Adversarial -> adversarial_synthesis specs 
    | Admissible -> admissible_synthesis specs 
  in

  Timer.log "exporting to aiger";
  write_aiger Sys.argv.(1) aig;
  Timer.log "exporting to verilog";
  write_verilog Sys.argv.(1) "Main" aig


  

  
    

  
