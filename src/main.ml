open Reglisse

let output_game = ref true
let output_product = ref true

let merge_outputs aiger = 
  match aiger.Aiger.outputs with 
  | [] -> failwith "in Reglisse.merge_outputs: no outputs"
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
let assume_admissible_synthesis games =
  if not (Hashtbl.mem games "Main")
  then raise NoMainModule;

  print_endline "assume admissible synthesis";
  let aig = Hashtbl.find games "Main" in
  let product_aig,product,controllables,winning = 
    Admissibility.compositional_synthesis [aig]
  in

  let initial_states = List.map Game.initial_state [aig] (*games*) in
  let initial = List.fold_left Cudd.bddAnd (Cudd.bddTrue()) initial_states in
  let initial_winning = Cudd.bddRestrict (Region.latch_configuration winning) initial in

  if Cudd.value initial_winning = 1 
  then print_endline "realizable"
  else if Cudd.value initial_winning = 0
  then failwith "unrealizable"
  else 
    (print_endline "problem determining if the initial state is winning";
     print_endline "writing BDDs in initial_winning.out initial_state.out and winning.out";
     Cudd.dumpDot "initial_winning.dot" initial_winning;
     Cudd.dumpDot "initial_state.dot" initial;
     Cudd.dumpDot "winning.dot" (Region.latch_configuration winning);
    );

  if !output_product
  then (
    Timer.log "writing product.aag";
    let aig = merge_outputs product_aig in
    let aig = make_controllable aig controllables in
    Aiger.write_to_file aig "product.aag";
  );

  Timer.log "computing strategy";
  let strategy = Strategy.of_region product winning in

  Timer.log "exporting to aiger";(*
  let contr = 
    Hashtbl.fold
      (fun accu (_,_,c,_,_) -> 
	List.fold_left (fun a b -> AigerBdd.VariableSet.add b a) accu c
      ) AigerBdd.VariableSet.empty games
  in

  let uncontr = 
    AigerBdd.VariableSet.diff
      (Hashtbl.fold
	 (fun accu (_,_,_,u,_) -> 
	   List.fold_left (fun a b -> AigerBdd.VariableSet.add b a) accu u
	 ) AigerBdd.VariableSet.empty games
      ) contr
  in*)
  let contr = List.fold_left (fun a b -> AigerBdd.VariableSet.add b a) AigerBdd.VariableSet.empty aig.contr in
  let uncontr = List.fold_left (fun a b -> AigerBdd.VariableSet.add b a) AigerBdd.VariableSet.empty aig.uncontr in

  let aig_strategy = Strategy.to_aiger product_aig strategy (AigerBdd.VariableSet.elements contr) (AigerBdd.VariableSet.elements uncontr) in
  Timer.log "assume_admissible_synthesis finished";
  aig_strategy

    
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
    
let main = 
  if Array.length Sys.argv < 2 then Printf.printf "usage: %s <file.rgl>\n" Sys.argv.(0);
  let file = Sys.argv.(1) in
  let inch = open_in file in
  let stream = Stream.of_channel inch in
  let token_stream = lexer stream in
  let specs =
    let rec loop accu =
      let spec =
	try
	  let spec = parse token_stream in Some spec 
	with NoMoreModule -> 
	  close_in inch; None
	| e ->
	  print_endline "Remaining tokens:";
	  print_endline (Common.remaining_tokens token_stream);
	  raise e

      in match spec with Some x -> loop (x :: accu) | None -> accu
    in 
    let res = loop [] in
    close_in inch;
    List.rev res

  in 

  Cudd.init 100;

  let tab_adm = Hashtbl.create 10 in
  let tab_game = Hashtbl.create 10 in
  let environment = Env.create (List.length specs) in

  List.fold_left
    (fun i modul -> 
     Timer.log ("module "^modul.module_name);
     (* be carreful here "never" gets an integer added *)
     let prefix = "never_"^string_of_int i in
     match safety_to_aiger ~prefix modul with
     | None -> 
	prerr_endline "Warning: compositional module"; 
	let Some aig = Reglisse.calls_to_aiger ~environment modul in
	let game = Game.of_aiger aig modul.inputs modul.outputs (prefix^"_accept") in
	Hashtbl.add tab_game modul.module_name game;
	write_aiger modul.module_name aig; 
	write_verilog modul.module_name modul.module_name aig;
	i+1
     | Some aiger ->
	let game = Game.of_aiger aiger modul.inputs modul.outputs (prefix^"_accept") in
	Hashtbl.add tab_game modul.module_name game;
	let value,adm = Admissibility.admissible_strategies game in
	Hashtbl.add tab_adm modul.module_name adm;
	Timer.log ("Value for module "^modul.module_name^" = "^string_of_int value);
	if value = 1 
	then 
	  let aig_strategy = Strategy.to_aiger aiger adm game.Game.contr game.Game.uncontr in
	  let aig_strategy = Aiger.hide aig_strategy (prefix^"_accept",Some 0) in
	  Reglisse.Env.add_module environment modul.module_name modul.inputs modul.outputs aig_strategy;
	  write_aiger modul.module_name aig_strategy; 
	  write_verilog modul.module_name modul.module_name aig_strategy		       
	else ();
	i+1
    ) 0 specs

  (* try let aig = assume_admissible_synthesis tab_game in
      write_aiger file aig;
      write_verilog file (List.hd specs).module_name aig
  with NoMainModule -> prerr_endline "Warning: no module Main"*)

  

  
    

  
