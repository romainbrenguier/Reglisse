open Expression

module StringSet = Set.Make(String)

module Proposition = 
struct 
  type t = 
  | Var of string * int
  | And of t * t
  | Or of t * t
  | Not of t
  | True | False

  let rec eval valuation = function
    | Var (s,i) -> valuation s = i
    | And (a,b) -> eval valuation a && eval valuation b
    | Or (a,b) -> eval valuation a || eval valuation b
    | Not a -> not (eval valuation a)
    | True -> true
    | False -> false

  let rec parse = 
    parser
    | [< a = parse_atomic; r = parse_prop_remainder a >] -> r
    | [< 'Genlex.Kwd "("; e = parse; 'Genlex.Kwd ")"; r = parse_prop_remainder e >] -> r
    | [< 'Genlex.Kwd "!"; e = parse_prop_not; r = parse_prop_remainder e >] -> r
  and parse_atomic = 
      parser
    | [< 'Genlex.Ident name >]
      (*'Genlex.Kwd "=" ; 'Genlex.Int i >] *) -> Var(name,1)
    | [< 'Genlex.Kwd "true" >] -> True
    | [< 'Genlex.Kwd "false" >] -> False
  and parse_prop_remainder e =
    parser
    | [< 'Genlex.Kwd "|";  f = parse >] -> Or(e,f)
    | [< 'Genlex.Kwd "&";  f = parse >] -> And(e,f)
    | [< 'Genlex.Kwd "<->";  f = parse >] -> Or (And(e,f),And(Not(e),Not(f)))
    | [< >] -> e
  and parse_prop_not =
    parser
    | [< 'Genlex.Ident name >]
      (* ; 'Genlex.Kwd "=" ; 'Genlex.Int i >]*) -> (Var(name,0))
    | [< 'Genlex.Kwd "("; e = parse; 'Genlex.Kwd ")" >] -> Not e

  let parse_keywords = ["=";"(";")";"!";"|";"&";"<->";"true";"false"]

  let rec to_string = function
    | Var (s,i) ->
       if i = 1 then s else if i = 0 then "(!"^s^")" else failwith "comparison with integers is not implemented"
    | And (a,b) -> "("^to_string a ^" & "^to_string b^")"
    | Or (a,b) -> "("^to_string a ^" | "^to_string b^")"
    | Not a -> "(!("^to_string a ^"))"
    | True -> " 1 "
    | False -> " 0 "

  let compare a b = compare a b

  let rec labels = function
    | Var (s,i) -> StringSet.singleton s
    | And (a,b) -> StringSet.union (labels a) (labels b)
    | Or (a,b) -> StringSet.union (labels a) (labels b)
    | Not a -> labels a
    | True | False -> StringSet.empty

  let to_speculog prop = 
    let rec aux = function 
      | Var (s,i) -> 
	 if i = 1 then s else if i = 0 then "(!"^s^")" else failwith "comparison with integers is not implemented"
      | And (a,b) -> "("^aux a ^" & "^aux b^")"
      | Or (a,b) -> "("^aux a ^" | "^aux b^")"
      | Not a -> "(!("^aux a ^"))"
      | True -> " 1 "
      | False -> " 0 "
    in aux prop

  let to_expression prop inputs = 
    let rec aux = function 
      | And (a,b) -> conj (aux a) (aux b)
      | Or (a,b) -> disj (aux a) (aux b)
      | Not a -> neg (aux a)
      | True -> bool true
      | False -> bool false
      | Var (s,i) -> 
	try
	  if i = 1 then List.assoc s inputs 
	  else if i = 0 then neg (List.assoc s inputs) else
	    failwith "comparison with integers is not implemented"
	with Not_found -> failwith ("variable "^s^" not found in RegularExpression.Proposition.to_expression "^to_string prop)

    in aux prop
    
  let to_bdd prop = 
    let rec aux = function
      | Var (s,i) -> 
	 (*Printf.printf "to_bdd: using lit %d for symbol %s\n" (Aiger.lit2int (Aiger.symbol2lit aiger (s,None))) s;*)
	let bdd_of_symbol = AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (AigerBdd.of_aiger_symbol (s,None))) in
	 if i = 1
	 then bdd_of_symbol
	 else if i = 0
	 then Cudd.bddNot bdd_of_symbol
	 else failwith "comparison with integers is not implemented"
      | And (a,b) -> Cudd.bddAnd (aux a) (aux b)
      | Or (a,b) ->  Cudd.bddOr (aux a) (aux b)
      | Not a -> Cudd.bddNot (aux a) 
      | True -> Cudd.bddTrue()
      | False -> Cudd.bddFalse()
    in aux prop

  let conj a b = match a,b with 
    | True , y | y , True -> y
    | False , _ | _ , False -> False
    | x , y -> And (x,y)

  let disj a b = match a,b with 
    | True , _ | _ , True -> True
    | False , y | y , False -> y
    | x , y -> Or (x,y)

  let neg a = match a with 
    | True -> False | False -> True | Not x -> x | x -> Not x
end

type t = 
| Prop of Proposition.t 
| Alt of t list
| Concat of t * t
| Star of t
| Epsilon
| Empty

let rec to_string = function
  | Alt a -> "("^ 
    List.fold_left (fun accu x -> accu ^"|"^to_string x) "" a
    ^")"
  | Concat (a,b) ->  "( "^to_string a ^" "^to_string b^" )"
  | Star a -> "("^to_string a ^"*)"
  | Epsilon -> " epsilon "
  | Empty -> " nothing "
  | Prop p -> " { "^Proposition.to_string p^" } " 



let rec accepts_epsilon = function
  | Alt l -> List.fold_left (fun accu x -> accepts_epsilon x || accu) false l
  | Concat (a,b) -> accepts_epsilon a && accepts_epsilon b
  | Star a -> true
  | Epsilon -> true
  | _ -> false

let disj a b = 
  let rec insert list x = match list with
    | [] -> [x]
    | hd :: tl -> if x < hd then x :: hd :: tl
      else if x = hd then hd :: tl 
      else hd :: insert tl x
  in
    match a,b with 
    | Empty, x -> x
    | x,Empty -> x
    | Epsilon, y | y , Epsilon when accepts_epsilon y -> y
    | Alt x, Alt y -> Alt(List.fold_left insert x y)
    | Alt x, y | y, Alt x -> Alt (insert x y)
    | x, y -> Alt (insert [y] x)

let alt = function
  | [] -> Empty
  | hd :: tl -> List.fold_left disj hd tl

let concat a b = match a,b with
  | Empty, _ -> Empty
  | _, Empty -> Empty
  | Epsilon,x -> x
  | x,Epsilon -> x
  | x,y -> Concat (x,y)

let star = function
  | Epsilon -> Epsilon
  | Empty -> Epsilon
  | x -> Star x

 
let seq el = List.fold_left concat Epsilon el
let plus e = concat e (star e)
let times e n = 
  let rec loop accu i = 
    if i = 0 then accu else loop (concat e accu) (i-1)
  in loop Epsilon n

let opt e = disj e Epsilon

let prop p = Prop p

let parse = 
  let rec parse = parser 
	| [< 'Genlex.Kwd "{"; p = Proposition.parse; 'Genlex.Kwd "}"; e = parse_remainder (Prop p) >] -> e
	| [< 'Genlex.Kwd "("; e = parse; 'Genlex.Kwd ")"; f = parse_remainder e >] -> f
	| [< 'Genlex.Kwd "."; e = parse_remainder (Prop (Proposition.True)) >] -> e
	| [< >] -> Epsilon
  and parse_remainder e = 
    parser
    | [< 'Genlex.Kwd "|"; p = parse >] -> disj e p
    | [< 'Genlex.Kwd "*"; p = parse >] -> concat (star e) p
    | [< 'Genlex.Kwd "+"; p = parse >] -> concat (concat e (star e)) p
    | [< 'Genlex.Kwd "?"; p = parse >] -> disj (concat e p) p
    | [< 'Genlex.Int i; p = parse >] -> concat (times e i) p 
    (*let rec loop i = if i = 1 then e else concat e (loop (i-1)) in 
     concat (loop i) p*)
    | [< p = parse >] -> concat e p
  in parser 
  | [< 'Genlex.Kwd "{"; p = Proposition.parse; 'Genlex.Kwd "}"; e = parse_remainder (Prop p) >] -> e
  | [< 'Genlex.Kwd "("; e = parse; 'Genlex.Kwd ")"; f = parse_remainder e >] -> f
  | [< 'Genlex.Kwd "."; e = parse_remainder (Prop (Proposition.True)) >] -> e
  | [< >] -> Empty (*Epsilon*)

let parse_keywords = List.rev_append ["{";"}";".";"?";"+";"*"] Proposition.parse_keywords

let of_string s = parse (Genlex.make_lexer parse_keywords (Stream.of_string s))


module Int = struct type t = int let compare = compare end
module IntMap = Map.Make(Int)
module IntSet = Set.Make(Int)

let rec labels = function 
  | Prop p -> Proposition.labels p
  | Alt l -> List.fold_left (fun set x -> StringSet.union (labels x) set) StringSet.empty l
  | Concat (a,b) -> StringSet.union (labels a) (labels b)
  | Star a -> labels a
  | Epsilon | Empty -> StringSet.empty


(* Label of automata with epsilon transitions *)
type label = EpsilonTrans | PropTrans of Proposition.t

type automaton = 
  {
    init: int list; 
    accept: int list; 
    transitions: ((label * int) list) IntMap.t 
  }

let automaton_to_string auto = 
  let buf = Buffer.create 200 in
  Printf.bprintf buf "INITIAL STATES:";
  List.iter (fun i -> Printf.bprintf buf "%d " i) auto.init;
  Printf.bprintf buf "\nACCEPTING STATES:";
  List.iter (fun i -> Printf.bprintf buf "%d " i) auto.accept;
  Printf.bprintf buf "\nTRANSITIONS:";
  IntMap.iter (fun k li_list ->
    List.iter (fun (l,i) ->
      match l with 
      | EpsilonTrans -> 
	Printf.bprintf buf "%d -> %d \n" k i
      | PropTrans p ->
	Printf.bprintf buf "%d -> %d {%s}\n" k i (Proposition.to_string p)
    ) li_list
  ) auto.transitions;
  Buffer.contents buf

let make_automaton ~init ~accept ~transitions =
  { init = init; accept = accept; transitions = transitions }

let add_trans s l t map = 
  let trans = 
    try IntMap.find s map
    with Not_found -> []
  in 
  IntMap.add s ((l,t)::trans) map 

let one_accept auto = 
  match auto.accept with
  | [a] -> a 
  | _ -> failwith "in RegularExpression.one_accept: the given automaton does not have a unique accepting state"

let one_init auto = 
  match auto.init with
  | [a] -> a 
  | _ -> failwith "in RegularExpression.one_init: the given automaton does not have a unique initial state"

(* Another method using non-deterministic automata*)
let non_det_of_expr expr = 
  let max_index = ref 0 in
  let new_index () = incr max_index; !max_index - 1 in
  let merge key a b = match a , b with
    | None, Some x | Some x , None -> Some x
    | _ -> failwith "in RegulareExpression.non_det_of_expr: the mappings contain two identical keys"
  in

  let rec aux = function
    | Alt a -> 
      let init = new_index () in
      let autos = List.map aux a in
      let accept = new_index () in
      (* adding transitions for each sub-automaton *)
      let transitions = List.fold_left 
	(fun accu auto ->
	  add_trans init EpsilonTrans (one_init auto)
	    (add_trans (one_accept auto) EpsilonTrans accept
	       (IntMap.merge merge accu auto.transitions))
	) IntMap.empty autos
      in 
      make_automaton ~init:[init] ~accept:[accept] ~transitions


    | Concat (a,b) ->
      let auto_a, auto_b = aux a, aux b in
      let init = one_init auto_a in
      let accept = one_accept auto_b in
      let transitions = 
	add_trans (one_accept auto_a) EpsilonTrans (one_init auto_b)
	  (IntMap.merge merge auto_a.transitions auto_b.transitions)
      in 
      make_automaton ~init:[init] ~accept:[accept] ~transitions
	
  | Star a -> 
      let init = new_index () in
      let auto = aux a in
      let accept = new_index () in
      let transitions = 
	add_trans init EpsilonTrans accept
	  (add_trans (one_accept auto) EpsilonTrans (one_init auto)
	     (add_trans init EpsilonTrans (one_init auto)
		(add_trans (one_accept auto) EpsilonTrans accept
		   auto.transitions)))
      in 
      make_automaton ~init:[init] ~accept:[accept] ~transitions

  | Epsilon ->
    let init = new_index () in
    let accept = init in
    let transitions = IntMap.empty in 
    make_automaton ~init:[init] ~accept:[accept] ~transitions

  | Empty -> 
    let init = new_index () in
    let accept = new_index () in
    let transitions = IntMap.empty in 
    make_automaton ~init:[init] ~accept:[accept] ~transitions

  | Prop p -> 
    let init = new_index () in
    let accept = new_index () in
    let transitions = IntMap.singleton init [PropTrans p, accept] in
    make_automaton ~init:[init] ~accept:[accept] ~transitions
  
  in aux expr  


let transitive_closure auto state =
  let rec aux reachable s = 
    let trans = try IntMap.find s auto.transitions 
      with Not_found -> 
	Common.debug ("warning: state "^string_of_int s^" not found"); 
	[] 
    in
    List.fold_left 
      (fun reachable (label, t) ->
	if label = EpsilonTrans && not (IntSet.mem t reachable)
	then aux (IntSet.add t reachable) t
	else reachable
      ) reachable trans
  in aux (IntSet.singleton state) state

(* Set.of_list is not present before ocaml 4.02 *)
let int_set_of_list =
  List.fold_left (fun accu elt -> IntSet.add elt accu) IntSet.empty

let states auto = 
  IntMap.fold 
    (fun k _ accu ->
      IntSet.add k accu
    ) auto.transitions
    (IntSet.union
       (int_set_of_list auto.init) (int_set_of_list auto.accept))
    
let remove_epsilon auto = 
  (* given a state, generates all the outgoing transitions *)
  let treat_state s trans =
    List.fold_left
      (fun accu (label,t) ->
	if label = EpsilonTrans 
	then accu 
	else
	  IntSet.fold 
	    (fun x accu ->
	      (label,x):: accu
	    ) (transitive_closure auto t) accu
      ) [] trans
  in

  let init = IntSet.elements (transitive_closure auto (one_init auto)) in
  
  let accept = 
    List.fold_left
      (fun accu s -> 
	if IntSet.mem (one_accept auto) (transitive_closure auto s)
	then (s :: accu) else accu
      ) [] (IntSet.elements (states auto))
  in

  let transitions = IntMap.mapi treat_state auto.transitions in
  make_automaton ~init ~accept ~transitions

(* takes as input an automaton without epsilon transitions *)
let coaccessible auto = 
  let all_trans = 
    IntMap.fold
      (fun s lt_list accu ->
       List.fold_left (fun accu (l,t) -> (s,t) :: accu) accu lt_list
      ) auto.transitions []
  in

  let rec loop coaccess =
    let new_coaccess = 
      List.fold_left 
	(fun accu (s,t) -> 
	 if IntSet.mem t accu
	 then IntSet.add s accu
	 else accu
	) coaccess all_trans
    in
    if IntSet.equal coaccess new_coaccess 
    then coaccess else loop new_coaccess
  in 

  let accept = auto.accept in

  let coreach = loop (int_set_of_list accept) in


  let init = List.filter (fun t -> IntSet.mem t coreach) auto.init in 

  let transitions = 
    IntMap.fold
      (fun k lt_list accu -> 
       if IntSet.mem k coreach
       then 
	 let list = List.filter (fun (l,t) -> IntSet.mem t coreach) lt_list in
	 IntMap.add k list accu
       else accu
      ) auto.transitions IntMap.empty
  in
  make_automaton ~init ~accept ~transitions

    

(* takes as input an automaton without epsilon transitions *)
let automaton_to_aiger ?(prefix="") auto =
  let state_var = 
    IntSet.fold 
      (fun state accu -> 
       IntMap.add state (var (prefix^"_state_"^string_of_int state) Type.bool)  accu
      ) (states auto) IntMap.empty
  in
  let init = var (prefix^"_init") Type.bool in
  let accept = var (prefix^"_accept") Type.bool in

  let acceptation = 
    List.fold_left 
      (fun accu s -> 
       Expression.disj accu (IntMap.find s state_var)
      ) (bool false) auto.accept
  in

  let label_set = 
    IntMap.fold 
      (fun s lt_list set ->
	List.fold_left 
	  (fun set (l,_) ->
	   match l with 
	   | EpsilonTrans -> failwith "in RegularExpression.automaton_to_aiger: the argument should have no epsilon transition"
	   | PropTrans p -> StringSet.union (Proposition.labels p) set
	  ) set lt_list
      ) auto.transitions StringSet.empty
  in
  let label_list = StringSet.fold (fun s a -> (s,var s Type.bool) :: a) label_set [] in
  
 (*maps states to there update expression*)
  let trans_map =
    IntMap.fold 
      (fun s lt_list map ->
	List.fold_left 
	  (fun map (l,t) ->
	    let new_p =
	      match l with 
	      | EpsilonTrans -> failwith "in RegularExpression.automaton_to_aiger: the argument should have no epsilon transition"
	      | PropTrans p ->
		 let new_expr = (conj (Proposition.to_expression p label_list) (IntMap.find s state_var)) in
		 try 
		   let old = IntMap.find t map in Expression.disj old new_expr
		 with Not_found -> new_expr
	    in IntMap.add t new_p map
	  ) map lt_list
      ) auto.transitions IntMap.empty
  in

  let initials = 
    IntSet.fold
      (fun s accu ->
       IntMap.add s (if List.mem s auto.init then bool true else bool false) accu
      ) (states auto) IntMap.empty
  in

  let aig_trans = 
  functional_synthesis 
    (
      [
	init, bool true;
	accept, acceptation
      ]
      @ 
	IntSet.fold
	  (fun s accu ->
	   (IntMap.find s state_var, 
	    ite init
		(try IntMap.find s trans_map
		 with Not_found -> 
		   Common.debug ("warning: transition for state "^string_of_int s^" not found"); bool false)
		(IntMap.find s initials))
	   :: accu
	  ) (states auto) []
    )
  in

  let aig = Aiger.hide aig_trans (prefix^"_accept",Some 0) in
  let aig, no_accept =
    List.fold_left 
      (fun (aig,gate) s -> 
       let aig, v = Aiger.new_var aig in
       let lit_state = Aiger.symbol2lit aig (prefix^"_state_"^string_of_int s,Some 0) in
       let aig = Aiger.add_and aig (Aiger.var2lit v) gate (Aiger.aiger_not lit_state) in
       aig, (Aiger.var2lit v)
      ) (aig,Aiger.aiger_true) auto.accept
  in

  let aig,var = Aiger.new_var aig in
  let lit = Aiger.var2lit var in
  let aig = Aiger.add_and aig lit (Aiger.aiger_not no_accept) Aiger.aiger_true in

  Aiger.add_output aig lit (prefix^"_accept",Some 0)
  (*Aiger.add_output aig (Aiger.aiger_not no_accept) (prefix^"accept",Some 0)*)
		 


let to_aiger ?(prefix="") expr =
  let auto = non_det_of_expr expr in
  Common.debug (automaton_to_string auto);
  let we_auto = remove_epsilon auto in
  Common.debug (automaton_to_string we_auto);
  let co_auto = coaccessible we_auto in
  Common.debug (automaton_to_string co_auto);
  Cudd.init 100;
  automaton_to_aiger ~prefix co_auto

let regexp string = to_aiger (of_string string)

let test string = 
  let aiger = regexp string in
  Aiger.write aiger stdout

let of_file file_name = 
  let inch = open_in file_name in
  let rec loop accu = 
    let line = try Some (input_line inch) with _ -> None in
    match line with None -> accu
		  | Some l -> loop (of_string l :: accu) 
  in 
  let regexps = loop [] in
  close_in inch;
  alt regexps

let main = 
  (* Common.display_debug := true;*)
  if Common.starts_with "./regularExpression" Sys.argv.(0) 
  then
    if Array.length Sys.argv < 2
    then
      Printf.printf "usage : %s regexp | %s file_name\n" Sys.argv.(0) Sys.argv.(0)
    else
      let regexp = 
	try of_file Sys.argv.(1)
	with _ -> of_string Sys.argv.(1)
      in 
      Aiger.write (to_aiger regexp) stdout


