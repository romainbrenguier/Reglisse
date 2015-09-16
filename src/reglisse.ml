(* Transforms regular expression specifications in HDL (Verilog or AIGER) *)

type safety = 
  | Not of string 
  | Never of string 
  | If_then of string * string 
  | Only_if of string * string
  | When of string * string 
  | If_then_else of string * string * string 
  | When_else of string * string * string 

let rec safety_to_expr = function 
  | Not s -> ExtendedExpression.of_string s
  | Never s -> ExtendedExpression.of_string ("{true} * ("^s^")")
  | If_then (if_string, then_string) ->
     let regexp_if = RegularExpression.of_string if_string in
     let regexp_then = Sequence.string_neg then_string in
     let exp = RegularExpression.concat regexp_if regexp_then in
     ExtendedExpression.regexp exp
  | Only_if (if_string, then_string) ->
     let regexp_if = Sequence.string_neg if_string in
     let regexp_then = RegularExpression.of_string then_string in
     let exp = RegularExpression.concat regexp_if regexp_then in
     ExtendedExpression.regexp exp
  | When (s,t) -> 
     safety_to_expr (If_then ("{true} * ("^s^")",t))
  | If_then_else (if_string, then_string, else_string) ->
     let regexp_if = RegularExpression.of_string if_string in
     let regexp_then = Sequence.string_neg then_string in
     let regexp_else = Sequence.string_neg else_string in
     let exp1 = RegularExpression.concat regexp_if regexp_then in
     ExtendedExpression.alt 
       [ ExtendedExpression.regexp exp1;
	 ExtendedExpression.concat 
	   (ExtendedExpression.neg (ExtendedExpression.regexp regexp_if))
	   (ExtendedExpression.regexp regexp_else)]
  | When_else (s,t,e) -> 
     safety_to_expr (If_then_else ("{true} * ("^s^")",t,e))
     

type t =
  { 
    module_name: string;
    inputs: string list; 
    outputs: string list;
    safety: safety list;
    eventually: string list;
    module_calls: (string * string list) list
  }

let new_module name = 
  {module_name=name;inputs=[];outputs=[];safety=[];eventually=[]; module_calls=[]}

let add_input m input = {m with inputs=input :: m.inputs}
let add_output m output = {m with outputs=output :: m.outputs}
let add_safety m never = {m with safety=never :: m.safety}
let add_eventually m eventually = {m with eventually=eventually :: m.eventually}
let add_call m c = { m with module_calls = c :: m.module_calls }


let lexer = Genlex.make_lexer
  ["module";"endmodule";"never";"enventually";
   "only";"if";"then";"else";"input";"output";"when";
   "(";")";",";";";"/*";"*/"]

exception NoMoreModule

let parse stream =

  let rec parse_comment = parser
  | [< 'Genlex.Kwd "*/" >] -> ()
  | [< >] -> Stream.junk stream; parse_comment stream
  in

  let parse_else = parser
  | [< 'Genlex.Kwd "else"; 'Genlex.String regexp; 'Genlex.Kwd ";" >] -> Some regexp
  | [< 'Genlex.Kwd ";" >] -> None
  in

  let rec parse_arguments accu = parser
  | [< 'Genlex.Ident a >] -> (parser
  | [< 'Genlex.Kwd "," >] -> parse_arguments (a :: accu) stream
  | [< 'Genlex.Kwd ")" >] -> a :: accu) stream
  in

  let rec parse_conditions m = parser
  | [< 'Genlex.Kwd "not" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
     parse_conditions (add_safety m (Not regexp)) stream
  | [< 'Genlex.Kwd "never" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
     parse_conditions (add_safety m (Never regexp)) stream
  | [< 'Genlex.Kwd "eventually" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
     failwith "eventually not supported"

  | [< 'Genlex.Kwd "if" ; 'Genlex.String regexp; 'Genlex.Kwd "then"; 'Genlex.String seq; x = parse_else >] ->
     (match x with None -> parse_conditions (add_safety m (If_then (regexp,seq))) stream
		| Some e -> parse_conditions (add_safety m (If_then_else (regexp,seq,e))) stream)

  | [< 'Genlex.Kwd "when" ; 'Genlex.String regexp; 'Genlex.Kwd "then"; 'Genlex.String seq; x = parse_else >] ->
     (match x  with None -> parse_conditions (add_safety m (When (regexp,seq))) stream
		  | Some e -> parse_conditions (add_safety m (When_else (regexp,seq,e))) stream)

  | [< 'Genlex.Kwd "only"; 'Genlex.Kwd "if" ; 'Genlex.String seq; 'Genlex.Kwd "then"; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
     parse_conditions (add_safety m (Only_if (seq,regexp))) stream

  | [< 'Genlex.Ident name; 'Genlex.Kwd "("; a = parse_arguments []; 'Genlex.Kwd ";" >] ->
     parse_conditions (add_call m (name, a)) stream
  | [< 'Genlex.Kwd "/*" >] -> parse_comment stream; parse_conditions m stream
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
    | [< 'Genlex.Kwd "/*" >] -> parse_comment stream;  aux stream
    | [< >] -> 
       Stream.empty stream;
       raise NoMoreModule

  (*failwith "in Reglisse.parse: stream should start with: [module module_name ( ]"*)

  in aux stream



let parse_file file = 
  let inch = open_in file in
  let stream = Stream.of_channel inch in
  let token_stream = lexer stream in
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


let safety_to_aiger ?(prefix="never") t = 
  if t.safety = [] then None
  else
    let expressions = List.map safety_to_expr t.safety in
    let expr = ExtendedExpression.alt expressions in
    Some (ExtendedExpression.to_aiger ~prefix expr)

let safety_to_game modul = 
  let prefix = "never_"^modul.module_name in
  match safety_to_aiger ~prefix modul with
  | Some aig -> Some ( Game.of_aiger ~aig ~inputs:modul.inputs ~outputs:modul.outputs ~errors:[prefix^"_accept"])
  | None -> None

module Env = 
struct
  type t = {arguments: (string, string list * string list) Hashtbl.t;
	    aigers: (string, Aiger.t) Hashtbl.t;
	    games: (string, Game.t) Hashtbl.t}
  let create i = { arguments=Hashtbl.create i; aigers=Hashtbl.create i; games=Hashtbl.create i}
  let default = create 1
  let new_module h name inputs outputs =
    Hashtbl.add h.arguments name (inputs,outputs)

  let add_aiger h name aiger = 
    Hashtbl.add h.aigers name aiger

  let add_game h name game = 
    Hashtbl.add h.games name game

  let find h x = Hashtbl.find h x
  let find_arguments_exn h name = 
    let (i,o) = find h.arguments name in o @ i

  let find_arguments h name = 
    try
      let (i,o) = find h.arguments name in Some (o @ i)
    with Not_found -> None

  let find_aiger_exn h name = find h.aigers name 
  let find_aiger h name = 
    try Some (find h.aigers name)
    with Not_found -> None

  let find_game_exn h name = find h.games name
  let find_game h name = 
    try Some (find h.games name)
    with Not_found -> None
end

let calls_to_aiger ?(env=Env.default) t = 
  if t.module_calls = [] then None
  else
    Some 
      (
	List.fold_left 
	  (fun aiger (module_name,arguments) ->
	   Common.debug ("Calling "^module_name);
	   let renaming = List.combine (Env.find_arguments_exn env module_name) arguments in
	   if !Common.display_debug
	   then List.iter (fun (a,b) -> Printf.printf "renaming %s into %s\n" a b) renaming;
	   (* prerr_endline "we should also rename hidden outputs? so that there are no collisions"; *)
	   let aig1 = Aiger.full_rename (Env.find_aiger_exn env module_name) renaming in
	   Aiger.compose aiger aig1 
	  ) Aiger.empty t.module_calls
      )


let add_prefix_to_aiger ~prefix ~aig =
  let open Aiger in
  let symbols = 
    SymbolMap.fold 
      (fun (s,i) l accu -> 
       SymbolMap.add (prefix^s,i) l accu
      ) aig.symbols SymbolMap.empty 
  in
  let abstract = LitMap.map (fun (s,i) -> (prefix^s,i)) aig.abstract in
  {aig with symbols; abstract }


let calls_to_game env modules t =
  let cnt = ref 0 in
  let prefix () = "call_"^string_of_int !cnt ^"_" in
  if t.module_calls = [] then None
  else
    let game_error_list = 
      List.map 
	(fun (module_name,arguments) ->
	 incr cnt;
	 Common.debug ("Calling "^module_name);
	 let m = Hashtbl.find modules module_name in
	 let prefix = prefix()^module_name in
	 match safety_to_aiger ~prefix m with
	 | None -> failwith "in Reglisse.calls_to_game: could not convert the safety condition to an AIG circuit"
	 | Some aiger -> 
	    let renaming = List.combine (Env.find_arguments_exn env module_name) arguments in
	    if !Common.display_debug
	    then List.iter (fun (a,b) -> Printf.printf "renaming %s into %s\n" a b) renaming;
	    Aiger.full_rename aiger renaming, (prefix^"_accept")
	) t.module_calls
    in
    let game_list = List.map fst game_error_list in
    let error_list = List.map snd game_error_list in
    let aig = List.fold_left Aiger.compose (List.hd game_list) (List.tl game_list) in
    let game = Game.of_aiger ~aig ~inputs:t.inputs ~outputs:t.outputs ~errors:error_list in
    Some game

  
