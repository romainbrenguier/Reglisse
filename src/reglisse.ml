(* Transforms regular expression specifications in HDL (Verilog or AIGER) *)

type t =
  { 
    module_name: string;
    inputs: string list; 
    outputs: string list;
    never: string list;
    eventually: string list;
  }

let new_module name = 
  {module_name=name;inputs=[];outputs=[];never=[];eventually=[]}

let add_input m input = {m with inputs=input :: m.inputs}
let add_output m output = {m with outputs=output :: m.outputs}
let add_never m never = {m with never=never :: m.never}
let add_eventually m eventually = {m with eventually=eventually :: m.eventually}

let lexer = Genlex.make_lexer
  ["module";"endmodule";"never";"enventually";
   "only";"if";"then";"input";"output";
   "(";")";",";";"]

exception NoMoreModule

let parse stream =
  let rec parse_conditions m = parser
| [< 'Genlex.Kwd "not" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
  parse_conditions (add_never m regexp) stream
| [< 'Genlex.Kwd "never" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
  parse_conditions (add_never m ("{true} * ("^regexp^")")) stream
| [< 'Genlex.Kwd "eventually" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
  parse_conditions (add_eventually m regexp) stream
| [< 'Genlex.Kwd "if" ; 'Genlex.String regexp; 'Genlex.Kwd "then"; 'Genlex.String seq; 'Genlex.Kwd ";" >] ->
  parse_conditions (add_never m ("("^regexp ^")"^ (Sequence.string_neg seq))) stream
| [< 'Genlex.Kwd "only"; 'Genlex.Kwd "if" ; 'Genlex.String seq; 'Genlex.Kwd "then"; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
  parse_conditions (add_never m ((Sequence.string_neg seq)^"("^regexp^")")) stream
| [< 'Genlex.Kwd "endmodule" >] -> m
| [< >] -> failwith "in Reglisse.parse: waiting for [never \"regexp string\";] or [endmodule]"
  in

  let rec parse_arguments m = parser
      | [< 'Genlex.Kwd "input"; 'Genlex.Ident i >] ->
	parse_next_argument (add_input m i) stream
      | [< 'Genlex.Kwd "output"; 'Genlex.Ident o >] ->
	parse_next_argument (add_output m o) stream
      | [< >] -> failwith "in Reglisse.parse: waiting for [input input_name] or [output output_name]"

  and parse_next_argument m = parser
      | [< 'Genlex.Kwd "," >] -> parse_arguments m stream
      | [< 'Genlex.Kwd ")"; 'Genlex.Kwd ";" >] -> parse_conditions m stream
      | [< >] -> failwith "in Reglisse.parse: waiting for [ ); ] or [ , ]"
  in

  let rec aux = parser
| [< 'Genlex.Kwd "module"; 'Genlex.Ident mn; 'Genlex.Kwd "(" >] ->
  parse_arguments (new_module mn) stream
| [< >] -> raise NoMoreModule
  (*failwith "in Reglisse.parse: stream should start with: [module module_name ( ]"*)

  in aux stream

let reglisse_to_aiger t = 
  let aiger1 = 
    if t.never = [] then None
    else 
      let expressions = List.map RegularExpression.of_string t.never in
      let expr = RegularExpression.alt expressions in
      Some (RegularExpression.to_aiger ~prefix:"never" expr)
  in
  let aiger2 = 
    if t.eventually = [] then None
    else
      let expressions = List.map RegularExpression.of_string t.eventually in
      let expr = RegularExpression.alt expressions in
      Some (RegularExpression.to_aiger ~prefix:"eventually" expr)
  in
  match aiger1,aiger2 with 
  | None, Some a | Some a, None -> a
  | Some a, Some b -> print_endline "warning: for now reachability conditions are not working";
    Aiger.compose a b
  | None, None -> failwith "in Reglisse.reglisse_to_aiger: no safety and no reachability conditions"


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


  
  

let safety_synthesis aiger inputs outputs =
  let contr,uncontr = control aiger inputs outputs in
  (*let v = AigerBdd.Variable.find (AigerBdd.of_aiger_symbol ("never_accept",Some 0)) in*)

  (*let bdd_of_lit lit = 
    AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol (Aiger.lit2symbol aiger lit)))
  in*)
  let unsafe = AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol ("never_accept",Some 0))) in

  let aigerBdd = AigerBdd.of_aiger aiger in
  let losing = Attractor.trap aigerBdd contr uncontr unsafe in
  let winning = Region.negation losing in

  let initial = Region.initial_state aiger in
  let initial_winning = Cudd.bddRestrict (Region.latch_configuration winning) initial in

  if Cudd.value initial_winning = 1 
  then print_endline "realizable"
  else if Cudd.value initial_winning = 0
  then failwith "unrealizable"
  else 
    (print_endline "problem determining if the initial state is winning";
     print_endline "writing BDDs in initial_winning.out initial_state.out and winning.out";
     Cudd.dumpDot ("initial_winning.out") initial_winning;
     Cudd.dumpDot ("initial_state.out") initial;
     Cudd.dumpDot ("winning.out") (Region.latch_configuration winning);
    );

  let strategy = Attractor.strategy aigerBdd winning in
  let aig_strategy = Attractor.strategy_to_aiger aiger strategy contr uncontr in
  aig_strategy





(*
let synthesis aiger inputs outputs = 
  print_endline "warning: for now reachability conditions are not working";
  let formula = WeakMuller.Formula.F_temp (WeakMuller.Formula.TF_always (StateFormula.AF_equal ("never_accept", 0))) in
  (*WeakMuller.Formula.F_and
      (WeakMuller.Formula.F_temp (WeakMuller.Formula.TF_eventually (StateFormula.AF_equal ("eventually_accept", 1))),
       WeakMuller.Formula.F_temp (WeakMuller.Formula.TF_always (StateFormula.AF_equal ("never_accept", 0)))
      )
  in*) 

  let contr,uncontr =  control aiger inputs outputs in
  let product, winning = WeakMuller.winning_region aiger contr uncontr formula false in
  print_endline "winning";
  let initial = Region.initial_state product in
  let initial_winning = Cudd.bddRestrict (Region.latch_configuration winning) initial in

  if Cudd.value initial_winning = 1 
  then print_endline "realizable"
  else if Cudd.value initial_winning = 0
  then print_endline "unrealizable"
  else Cudd.dumpDot ("initial_winning.out") initial_winning  

*)
    
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
	  let spec = parse token_stream in
	  Some spec 
	with NoMoreModule -> 
	  close_in inch;
	  None
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
  let synth = 
    match specs with 
    | [spec] -> 
      let aiger = reglisse_to_aiger spec in
      safety_synthesis aiger spec.inputs spec.outputs
    | spec_list ->
      let aigers = 
	List.map (fun x -> 
	  let aiger = reglisse_to_aiger x in
	  let aigerBdd = AigerBdd.of_aiger aiger in
	  let unsafe = AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol ("never_accept",Some 0))) in 
	  let contr,uncontr = control aiger x.inputs x.outputs in
	  aiger,aigerBdd, contr, uncontr, (Region.of_bdds unsafe (Cudd.bddTrue()))
	) spec_list
      in
      let product,winning = Admissibility.compositional_synthesis aigers
      in
      let strategy = Attractor.strategy product winning in
      (*let aig_strategy = Attractor.strategy_to_aiger product strategy contr uncontr in
      aig_strategy*)
      let initial_states = List.map (fun (a,_,_,_,_) -> Region.initial_state a) aigers in
      let initial = List.fold_left Cudd.bddAnd (Cudd.bddTrue()) initial_states in
      let initial_winning = Cudd.bddRestrict (Region.latch_configuration winning) initial in
      
      if Cudd.value initial_winning = 1 
      then failwith "realizable"
      else if Cudd.value initial_winning = 0
      then failwith "unrealizable"
      else failwith "problem..."

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
    

  
