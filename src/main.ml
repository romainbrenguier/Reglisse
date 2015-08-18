open Reglisse

let output_game = ref true
let output_product = ref true


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


let assume_admissible_synthesis games =
  print_endline "assume admissible synthesis";
  let aig = Hashtbl.find games "Main" in
  let product_aig,product,controllables,winning = 
    Admissibility.compositional_synthesis [aig]
  in
 

  let initial_states = List.map (fun (a,_,_,_,_) -> Region.initial_state a) [aig] (*games*) in
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
  let strategy = Attractor.strategy product winning in

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
  let (_,_,c,u,_) = aig in
  let contr = List.fold_left (fun a b -> AigerBdd.VariableSet.add b a) AigerBdd.VariableSet.empty c in
  let uncontr = List.fold_left (fun a b -> AigerBdd.VariableSet.add b a) AigerBdd.VariableSet.empty u in

  let aig_strategy = Attractor.strategy_to_aiger product_aig strategy (AigerBdd.VariableSet.elements contr) (AigerBdd.VariableSet.elements uncontr) in
  Timer.log "assume_admissible_synthesis finished";
  aig_strategy

    
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

  let synth = 
    let tab_adm = Hashtbl.create 10 in
    let tab_game = Hashtbl.create 10 in
    let _ = 
      List.fold_left
	(fun i modul -> 
	 Printf.printf "module %s\n" modul.module_name;
	 let prefix = "never_"^string_of_int i in
	 let aiger = reglisse_to_aiger ~prefix modul in
	 let aigerBdd = AigerBdd.Circuit.of_aiger aiger in
	 let unsafe = AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol (prefix^"_accept",Some 0))) in 
	 (* be carreful here "never" gets an integer added *)
	 let contr,uncontr = control aiger modul.inputs modul.outputs in
	 let game = (aiger,aigerBdd, contr, uncontr, unsafe) in
	 Hashtbl.add tab_game modul.module_name game;
	 let value,adm = Admissibility.admissible_strategies game in
	 Hashtbl.add tab_adm modul.module_name adm;

	 Printf.printf "Value for module %s = %d\n" modul.module_name value;
	 i+1
	) 0 specs
    in
    assume_admissible_synthesis tab_game


  in	
  let output_file = file^".aag" in
  print_endline ("writing aiger in "^output_file);
  let outch = open_out output_file in
  Aiger.write synth outch;
  close_out outch;
  
  let output_file = file^".v" in
  print_endline ("writing verilog in "^output_file);
  let outch = open_out output_file in
  Verilog.of_aiger (List.hd specs).module_name synth outch;
  close_out outch
    

  
