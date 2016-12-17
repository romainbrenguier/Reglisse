(* To compile this file use: 
   ocamlbuild -tag camlp4 -tag use_ocaml-cudd -tag use_ocaml-aiger acacia2aig.byte
*)
open Speculoos
open Expression

let lexer = Genlex.make_lexer ["State"; "initial"; ":"; "("; ")"; ","; "!"; "&&"; "U"; "T"; "F"; "||"; "{"; "}"; "to"; "state"; "labeled"]

type conjunction = (bool * string) list
type dnf_formula = conjunction list
type state = { id: int; init: bool; trans: (int * (dnf_formula * conjunction) list) list }

let parse = 
  let tab_variables = Hashtbl.create 10 in
  
  let is_initial = parser 
    | [< 'Genlex.Kwd ","; 'Genlex.Kwd "initial" >] -> true
    | [< >] -> false
  in
 

  let rec parse_conjunction accu = parser
    | [< 'Genlex.Kwd "!"; e = parse_var_conjunction;
	 f = parse_remainder_conjunction (match e with None -> accu 
	 | Some v ->  ((false,v)::accu))
      >] -> f
    | [< e = parse_var_conjunction; f = parse_remainder_conjunction
      (match e with None ->  accu 
      | Some v -> (true,v)::accu)
      >] -> f 
	
  and parse_var_conjunction = parser
      | [< 'Genlex.Kwd "T" >] -> None 
      | [< 'Genlex.Kwd "F" >] -> failwith "Acacia2aig: should not encounter false in a conjunction"
      | [< 'Genlex.Ident v >] ->  
	if Hashtbl.mem tab_variables v then Some v
	else
	  let var = Expression.var v Type.bool in
	  Hashtbl.add tab_variables v var;
	  Some v

  and parse_remainder_conjunction accu = parser
      | [< 'Genlex.Kwd "&&"; e = parse_conjunction accu >] -> e
      | [< >] -> accu
  in

  let rec parse_dnf accu = parser
   | [< 'Genlex.Kwd "("; e = parse_conjunction []; 'Genlex.Kwd ")"; f = parse_remainder_dnf (e::accu) >] -> f
   | [< e = parse_conjunction []; f = parse_remainder_dnf (e::accu) >] -> f
  and parse_remainder_dnf accu = parser
      | [< 'Genlex.Kwd "||"; e = parse_dnf accu >] -> e
      | [< >] -> accu
  in

  let parse_implication = parser
    | [< 'Genlex.Kwd "("; e = parse_dnf [] ; 'Genlex.Kwd ")"; 'Genlex.Kwd "U"; 'Genlex.Kwd "("; f = parse_conjunction []; 'Genlex.Kwd ")" >] -> (e,f)
  in

  let rec parse_label accu = parser
    | [< 'Genlex.Kwd "("; e = parse_implication ; 'Genlex.Kwd ")"; f = parse_remainder_label (e :: accu) >] -> f
  and parse_remainder_label accu = parser
      | [< 'Genlex.Kwd "||"; e = parse_label accu >] -> e
      | [< >] -> accu
  in

      
  let rec parse_transitions accu = parser
    | [< 'Genlex.Kwd "to"; 'Genlex.Kwd "state"; 'Genlex.Int i; 'Genlex.Kwd "labeled"; e = parse_label []; list = parse_transitions ((i,e):: accu) >] -> list
    | [< >] -> accu
  in

  let parse_state = parser
    | [< 'Genlex.Int s; i = is_initial; 'Genlex.Kwd ":"; 'Genlex.Ident outgoing; 'Genlex.Ident transitions; 'Genlex.Kwd ":"; list = parse_transitions [] >] -> {id=s;init=i;trans=list}
  in
  
  let rec parse_transition_system accu = parser
    | [< 'Genlex.Kwd "State"; s = parse_state; f = parse_transition_system (s::accu) >] -> f
    | [< >] -> accu
  in

  parser
  | [< 'Genlex.Ident transition; 'Genlex.Ident system; 'Genlex.Kwd "{"; e=parse_transition_system []; 'Genlex.Kwd "}" >] ->  e, tab_variables


let conjunction_to_expr string_to_var = 
  List.fold_left (fun e (b,x) -> if b then e $& string_to_var x else e $& neg (string_to_var x)) (Expression.bool true) 

let dnf_to_expr string_to_var = 
  List.fold_left (fun e c -> e $| conjunction_to_expr string_to_var c) (Expression.bool false)

    
let to_speculog (transition_system,tab_variables) = 
  let nb_states = List.length transition_system in
  let tab = Hashtbl.create nb_states in

  let output_tab = Hashtbl.create nb_states in

  let state_var = Expression.var "state" (Type.int (Common.log nb_states)) in
  
  List.iter 
    (fun state ->
      if state.init && state.id <> 0 then failwith "initial state is different from 0";
      if state.id >= nb_states then failwith "index of state is greater than the number of states";
      List.iter
	(fun (target,update_list) ->
	  List.iter (fun (inputs,outputs) ->
	    (*let expr = List.fold_left (fun e (b,x) -> if b then e $& x else e $& neg x) (Expression.bool true) inputs in*)
	    let expr = dnf_to_expr (Hashtbl.find tab_variables) inputs in
	    let s_and_expr = expr $& (state_var $= Expression.int state.id) in
	    List.iter (fun (pos,out) ->
	      if pos 
	      then 
		try 
		  let p = Hashtbl.find output_tab out in
		  Hashtbl.replace output_tab out (p $| s_and_expr)
		with Not_found -> Hashtbl.add output_tab out s_and_expr
	      else
		if not (Hashtbl.mem output_tab out)
		then Hashtbl.replace output_tab out (bool false)
	    ) outputs;
	    try 
	      let p = Hashtbl.find tab target in
	      Hashtbl.replace tab target 
		(p $| s_and_expr)
	    with Not_found -> Hashtbl.add tab target s_and_expr
	  ) update_list
	) state.trans
    ) transition_system;

  let update_state =
    Hashtbl.fold (fun k e accu -> 
      Expression.ite e (Expression.int k) accu) tab (Expression.int 0)
  in
  
  Hashtbl.fold (fun k e accu -> add_update accu (Hashtbl.find tab_variables k) e) output_tab  (Update (state_var, update_state))
    
      

let main = 
  let inch = Sys.argv.(1) |> open_in in
  let stream = inch |> Stream.of_channel |> lexer in
  try
    let spec = parse stream in 
    close_in inch;
    Cudd.init 30;
    spec |> to_speculog |> compile ;
    Cudd.quit ()
  with Stream.Error _ -> Printf.eprintf "%s\n" (Parser.remaining_tokens stream)

  
