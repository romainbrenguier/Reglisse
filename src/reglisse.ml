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

module Program = 
struct
  type t = 
  | Skip
  | SimpleExpr of Proposition.t
  | Concat of t * t
  | While of Proposition.t * t
  | If_then_else of Proposition.t * t * t
  (* | Parallel of t * t*)

  let skip = Skip
  let prop p = SimpleExpr p
  let concat a b = Concat(a,b)
  let make_while a b = While(a,b)
  let if_then_else a b c = If_then_else (a,b,c)
  (* let parallel a b = Parallel (a,b)*)

  module RE = RegularExpression

  let to_extended_expression = 
    let rec aux = function
      | Skip -> RE.Epsilon
      | SimpleExpr p -> RE.prop p
      | Concat (a,b) -> RE.concat (aux a) (aux b)
      | While(p,a) -> RE.concat (RE.star (RE.concat (RE.prop p) (aux a))) (RE.prop (Proposition.neg p))
      | If_then_else (p,a,b) -> RE.concat (RE.concat (RE.prop p) (aux a)) (RE.concat (RE.prop (Proposition.neg p)) (aux b))
    in aux
end

type module_content = 
| Calls of (string * string list) list
| Safety of safety list
| Eventually of Program.t list
| Functional of (string * Expression.t) list * (Expression.t * Expression.t) list
| Empty

type t = 
  { 
    module_name: string;
    inputs: string list; 
    outputs: string list;
    content: module_content;
  }

let new_module name = 
  {module_name=name;inputs=[];outputs=[];content=Empty}

let add_input m input = {m with inputs=input :: m.inputs}
let add_output m output = {m with outputs=output :: m.outputs}
let add_safety m never = 
  match m.content with 
  | Safety s -> {m with content = Safety (never :: s)}
  | Empty ->  {m with content = Safety [never]}
  | _ -> failwith "in Reglisse.add_safety: the given module is not an atomic module."

let add_program m eventually = 
  match m.content with 
  | Eventually p -> {m with content = Eventually (eventually :: p)}
  | Empty ->  {m with content = Eventually [eventually]}
  | _ -> failwith "in Reglisse.add_program: the given module is not an atomic module."


let add_call m c = 
  match m.content with 
  | Calls cs -> {m with content = Calls (c :: cs)}
  | Empty ->  {m with content = Calls [c]}
  | _ -> failwith "in Reglisse.add_call: the given module is not a compositional module."

let initialize_functional m = match m.content with 
  | Empty ->  
    let expr_inputs = List.map (fun i -> (i,Expression.var i Type.bool)) m.inputs in
    let expr_outputs = List.map (fun o -> (o,Expression.var o Type.bool)) m.outputs in
    {m with content = Functional (List.rev_append expr_inputs expr_outputs,[])}
  | Functional _ -> m
  | _ -> failwith "in Reglisse.initialize_functional: the given module is not a functional module."

let rec add_boolean_var m v = match m.content with
  | Functional (vars,ups) -> {m with content=Functional((v,Expression.var v Type.bool) :: vars, ups)}
  | Empty -> let m = initialize_functional m in add_boolean_var m v

let rec add_update m up =
  match m.content with 
  | Functional (v,ups) -> {m with content = Functional (v,up :: ups)}
  | Empty -> let m = initialize_functional m in add_update m up
  | _ -> failwith "in Reglisse.add_update: the given module is not a functional module."


let find_functional_var m v = 
  match m.content with
  | Functional (vars,_) ->
    (try List.assoc v vars 
     with Not_found ->
       failwith "in Reglisse.find_functional_var: the variable has not been declared."
    )
  | _ -> failwith "in Reglisse.find_functional_var: the given module is not a functional module."

let is_atomic m = match m.content with Calls _ | Empty -> true | _ -> false

let lexer = Genlex.make_lexer
  ["module";"endmodule";"never";"enventually";
   "only";"if";"then";"else";"input";"output";"reg";"when";"always";"not";
   "(";")";",";";";"/*";"*/";
   "<-"; "~"; "&"; "|"; "^";"true";"false"
  ]

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

  let rec parse_expression_aux m accu = parser 
  | [< 'Genlex.Kwd "&"; e = parse_expression m >] -> parse_expression_aux m (Expression.conj accu e) stream
  | [< 'Genlex.Kwd "|"; e = parse_expression m >] -> parse_expression_aux m (Expression.disj accu e) stream
  | [< 'Genlex.Kwd "^"; e = parse_expression m >] -> parse_expression_aux m (Expression.xor accu e) stream
  | [< >] -> accu
  and parse_expression m = parser
      | [< 'Genlex.Kwd "("; e = parse_expression m; 'Genlex.Kwd ")" >] -> parse_expression_aux m e stream
  | [< 'Genlex.Kwd "true" >] -> parse_expression_aux m (Expression.bool true) stream
  | [< 'Genlex.Kwd "false" >] -> parse_expression_aux m (Expression.bool false) stream
  | [< 'Genlex.Ident i >] -> parse_expression_aux m (find_functional_var m i) stream
    (* warning: ~ doesn't have high precedence *)
  | [< 'Genlex.Kwd "~"; e = parse_expression m >] -> Expression.neg e
  in

  let parse_call_or_update m ident = parser 
  | [< 'Genlex.Kwd "("; a = parse_arguments []; 'Genlex.Kwd ";" >] ->
    add_call m (ident, a)
  | [< 'Genlex.Kwd "<-" >] ->
    let m = initialize_functional m in
    let e = parse_expression m stream in
    (parser | [< 'Genlex.Kwd ";" >] -> 
     add_update m (find_functional_var m ident,e)) stream

  in

  let parse_program = parser 
  | [< >] ->  failwith "program parsing not implemented"
  in

  let rec parse_conditions m = parser
  (*| [< 'Genlex.Kwd "not"; 'Genlex.Kwd "always" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
    parse_conditions (add_safety m (Not (Printf.sprintf "( %s ) %d" regexp !not_always_bound))) stream*)
  | [< 'Genlex.Kwd "not" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
     parse_conditions (add_safety m (Not regexp)) stream

  | [< 'Genlex.Kwd "never" ; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
     parse_conditions (add_safety m (Never regexp)) stream
  | [< 'Genlex.Kwd "eventually" ; p = parse_program; 'Genlex.Kwd ";" >] ->
    parse_conditions (add_program m p) stream

  | [< 'Genlex.Kwd "if" ; 'Genlex.String regexp; 'Genlex.Kwd "then"; 'Genlex.String seq; x = parse_else >] ->
     (match x with None -> parse_conditions (add_safety m (If_then (regexp,seq))) stream
		| Some e -> parse_conditions (add_safety m (If_then_else (regexp,seq,e))) stream)

  | [< 'Genlex.Kwd "when" ; 'Genlex.String regexp; 'Genlex.Kwd "then"; 'Genlex.String seq; x = parse_else >] ->
     (match x  with None -> parse_conditions (add_safety m (When (regexp,seq))) stream
		  | Some e -> parse_conditions (add_safety m (When_else (regexp,seq,e))) stream)

  | [< 'Genlex.Kwd "only"; 'Genlex.Kwd "if" ; 'Genlex.String seq; 'Genlex.Kwd "then"; 'Genlex.String regexp; 'Genlex.Kwd ";" >] ->
     parse_conditions (add_safety m (Only_if (seq,regexp))) stream

  | [< 'Genlex.Ident name >] -> parse_conditions (parse_call_or_update m name stream) stream
  | [< 'Genlex.Kwd "reg"; 'Genlex.Ident name; 'Genlex.Kwd ";" >] -> parse_conditions (add_boolean_var m name) stream
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
  match t.content with
  | Safety s ->
    let expressions = List.map safety_to_expr s in
    let expr = ExtendedExpression.alt expressions in
    Some (ExtendedExpression.to_aiger ~prefix expr)
  | _ -> None


let safety_to_game modul = 
  let prefix = "never_"^modul.module_name in
  match safety_to_aiger ~prefix modul with
  | Some aig ->
     (Timer.log "Game generated from safety condition";
      Some (Game.of_aiger ~aig ~inputs:modul.inputs ~outputs:modul.outputs ~errors:[prefix^"_accept"]))
  | None -> None

let functional_to_aiger t = 
  match t.content with 
  | Functional (vars,updates) ->
     Some (Speculoos.to_aig (Speculoos.Seq (List.map (fun (a,b) -> Speculoos.Update(a,b)) updates)))
  | _ -> None 


module Env = 
struct
  type t = {arguments: (string, string list * string list) Hashtbl.t;
	    aigers: (string, AigerImperative.t) Hashtbl.t;
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
  match t.content with
  | Calls module_calls ->
    Some 
      (
	List.fold_left 
	  (fun aiger (module_name,arguments) ->
	    ReglisseCommon.debug ("Calling "^module_name);
	    let renaming = List.combine (Env.find_arguments_exn env module_name) arguments in
	   if !ReglisseCommon.display_debug
	   then List.iter (fun (a,b) -> Printf.printf "renaming %s into %s\n" a b) renaming;
	   (* prerr_endline "we should also rename hidden outputs? so that there are no collisions"; *)
	    let module_aig = Env.find_aiger_exn env module_name in
	    AigerImperative.rename module_aig (fun x -> try List.assoc x renaming with Not_found -> x);
	    AigerImperative.compose aiger module_aig 
	  ) (AigerImperative.empty()) module_calls
      )
    | _ -> None

let add_prefix_to_aiger ~prefix ~aig =
  AigerImperative.rename aig (fun x -> prefix^x) 
  (*let open Aiger in
  let symbols = 
    SymbolMap.fold 
      (fun (s,i) l accu -> 
	SymbolMap.add (prefix^s,i) l accu
      ) aig.symbols SymbolMap.empty 
  in
  let abstract = LitMap.map (fun (s,i) -> (prefix^s,i)) aig.abstract in
  {aig with symbols; abstract }*)

type call_renaming = {call:string; renaming:(string * string) list}

let calls_to_game env modules t =
  let cnt = ref 0 in
  let prefix () = "call_"^string_of_int !cnt ^"_" in
  match t.content with
  | Calls module_calls ->
    let game_error_list = 
      List.map 
	(fun (module_name,arguments) ->
	 incr cnt;
	 ReglisseCommon.debug ("Calling "^module_name);
	 let m = Hashtbl.find modules module_name in
	 let prefix = prefix()^module_name in
	 match safety_to_aiger ~prefix m with
	 | None -> failwith "in Reglisse.calls_to_game: could not convert the safety condition to an AIG circuit"
	 | Some aiger -> 
	    let renaming = List.combine (Env.find_arguments_exn env module_name) arguments in
	    if !ReglisseCommon.display_debug
	    then List.iter (fun (a,b) -> Printf.printf "renaming %s into %s\n" a b) renaming;
	    AigerImperative.rename aiger (fun x -> try List.assoc x renaming with Not_found -> x);
	    aiger, {call=module_name;renaming}, (prefix^"_accept")
	) module_calls
    in
    let game_list,renaming_list,error_list = 
      List.fold_left (fun (la,lb,lc) (a,b,c) -> a::la,b::lb,c::lc) ([],[],[]) game_error_list 
    in
    let aig = List.fold_left AigerImperative.compose (List.hd game_list) (List.tl game_list) in
    let game = Game.of_aiger ~aig ~inputs:t.inputs ~outputs:t.outputs ~errors:error_list in
    Some (game,renaming_list)
  | _ -> Timer.warning "no module calls"; None

  
