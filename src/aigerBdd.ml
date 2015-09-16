(** This module defines usefull function for going from the aiger representation 
 * to the BDD representation and reciproqually *)

let init aiger =
  try Cudd.init (aiger.Aiger.num_inputs+2*aiger.Aiger.num_latches+2)
  with Failure x -> Printf.eprintf "warning: %s\n" x

type symbol = string * int
let of_aiger_symbol = function
  | name, Some i -> name, i
  | name, None -> name, 0
let to_aiger_symbol (n,i) = n,Some i
let symbol_to_string (n,i) = n^"<"^string_of_int i^">"

module SymbolMap = Map.Make(struct type t = symbol let compare = compare end)
module SymbolSet = Set.Make(struct type t = symbol let compare = compare end)

module Variable =
struct
  type t = int


  let compare a b = a - b
  let last_variable = ref 0
  let table_variable = Hashtbl.create 20
  let reverse_table = Hashtbl.create 20

  let to_string v = "Variable("^string_of_int v^","^symbol_to_string (Hashtbl.find reverse_table v)^")"

  let new_variable symbol = 
    let i = !last_variable in
    last_variable := !last_variable + 2;
    Hashtbl.add table_variable symbol i;
    Hashtbl.add reverse_table i symbol;
    (* (* Debug *) Printf.printf "new variable %s -> %d\n" (symbol_to_string symbol) i; *)
    i
    
  let find (l:symbol) = 
    try Hashtbl.find table_variable l 
    with Not_found -> new_variable l

  let to_bdd var = Cudd.ithVar var

  let next var = 
    if var mod 2 = 1
    then failwith "variables is already next"
    else var + 1

  let make_cube var_list = Cudd.make_cube var_list

  let to_int v = v

  let of_lit aiger lit = 
    find (of_aiger_symbol (Aiger.lit2symbol aiger lit))

  let max_var () = !last_variable
end

type variable = Variable.t

module VariableMap = Map.Make(Variable)
module VariableSet = Set.Make(Variable)


let map_of_aiger aiger = 
  let m =
    List.fold_left
      (fun m inp -> 
       let sym = Aiger.lit2symbol aiger inp in
       (*Printf.printf "adding to map input %d (%s) -> %d\n" (Aiger.lit2int inp) (Aiger.Symbol.to_string sym) (Variable.find (of_aiger_symbol sym)); *)

       VariableMap.add (Variable.find (of_aiger_symbol sym)) inp m
      ) VariableMap.empty aiger.Aiger.inputs
  in 
  List.fold_left
    (fun m (l,_) -> 
     let sym = Aiger.lit2symbol aiger l in
     (*Printf.printf "adding to map latch %d (%s) -> %d\n" (Aiger.lit2int l) (Aiger.Symbol.to_string sym) (Variable.find (of_aiger_symbol sym));*)
     VariableMap.add (Variable.find (of_aiger_symbol sym)) l m
    ) m aiger.Aiger.latches


let bdd_to_valuations bdd variables = 
  let rec aux bdd valuations = function 
    | [] -> valuations
    | var :: s ->
      let restricted_true = Cudd.bddRestrict bdd (Variable.to_bdd var) in
      let restricted_false = Cudd.bddRestrict bdd (Cudd.bddNot (Variable.to_bdd var)) in
      if Cudd.equal restricted_true (Cudd.bddFalse ())
      then 
	aux restricted_false (List.map (VariableMap.add var false) valuations) s
      else
	if Cudd.equal restricted_false (Cudd.bddFalse ())
	then 
	  aux restricted_true (List.map (VariableMap.add var true) valuations) s
	else
	  let a_true = aux restricted_true (List.map (VariableMap.add var true) valuations) s in
	  let a_false = aux restricted_false (List.map (VariableMap.add var false) valuations) s
	  in List.rev_append a_true a_false
  in  
  if Cudd.equal bdd (Cudd.bddFalse())
  then []
  else aux bdd [VariableMap.empty] variables

let map_to_string map = 
  VariableMap.fold 
    (fun var lit accu -> 
      accu^Variable.to_string var^" -> "^string_of_int (Aiger.lit2int lit)^"; "
    ) map ""

exception UndeclaredLit of Aiger.lit

module BddMap = Map.Make(struct type t = Cudd.bdd let compare = Cudd.compare end)

(* We should normalize the cache: ie no negated nodes *)
let add_bdd_to_aiger aiger v2l bdd = 
  let v2l = VariableMap.merge 
	      (fun k a b -> match a,b with 
			  | Some x , _ | None , Some x -> Some x
			  | _ -> None
	      ) v2l (map_of_aiger aiger) in
  let cache = BddMap.empty in

  let rec aux bdd aig cache = 
    try (aig, BddMap.find bdd cache,cache)
    with Not_found ->
      if Cudd.isConstant bdd
      then
	if Cudd.value bdd = 1
	then (aig,Aiger.aiger_true,cache)
	else (aig,Aiger.aiger_false,cache)
      else
	let variable = Cudd.nodeReadIndex bdd in
	let lit = 
	  try VariableMap.find variable v2l 
	  with Not_found -> 
	    failwith ("In AigerBdd.add_bdd_to_aiger: variable "^string_of_int variable^" not found in the given Aiger.lit VariableMap.t")
	in
	let then_child = Cudd.t bdd in
	let else_child = Cudd.e bdd in
	let (aig,then_lit,cache) = aux then_child aig cache in
	let (aig,else_lit,cache) = aux else_child aig cache in

	let aig, lit1 =
	  if then_lit = Aiger.aiger_false 
	  then aig, Aiger.aiger_false
	  else if then_lit = Aiger.aiger_true
	  then aig, lit
	  else
	    let aig,var1 = Aiger.new_var aig in 
	    let lit1 = Aiger.var2lit var1 in
	    let aig = Aiger.add_and aig lit1 lit then_lit in
	    aig, lit1
	in

	let aig, lit2 =
	  if else_lit = Aiger.aiger_false 
	  then aig, Aiger.aiger_false
	  else if else_lit = Aiger.aiger_true
	  then aig, Aiger.aiger_not lit
	  else
	    let aig,var2 = Aiger.new_var aig in 
	    let lit2 = Aiger.var2lit var2 in
	    let aig = Aiger.add_and aig lit2 (Aiger.aiger_not lit) else_lit in
	    aig, lit2
	in

	let aig,var3 = Aiger.new_var aig in
	let lit3 = Aiger.var2lit var3 in
	let aig = Aiger.add_and aig lit3 (Aiger.aiger_not lit1) (Aiger.aiger_not lit2) in
	
	let res = 
	  if Cudd.isComplement bdd then lit3 else Aiger.aiger_not lit3
	in
	
	(aig,res,BddMap.add bdd res cache)

  in 
  let aig,res,_ = aux bdd aiger cache in
  aig,res


let bdds_to_aiger ~inputs ~latches ~outputs =
  (* mapping variable to litterals *)
  let v2l = VariableMap.empty in
  let aig = Aiger.empty in
  let aig = 
    List.fold_left
      (fun aig i -> 
       (* (* Debug *) Printf.eprintf "adding %s<%d>\n" (fst i) (snd i);  *)
       let aig,var = Aiger.new_var aig in
       let lit = Aiger.var2lit var in
       (* VariableMap.add (Variable.find i) lit v2l,*)
       Aiger.add_input aig lit (to_aiger_symbol i)
      ) aig inputs
  in
  let v2l,aig = 
    List.fold_left
      (fun (v2l,aig) (l,_) -> 
       (* (* Debug *) Printf.eprintf "adding %s<%d>\n" (fst l) (snd l);*)
       let aig,var = Aiger.new_var aig in
       let lit = Aiger.var2lit var in
       (* Hashtbl.add mapping l lit;*)
       VariableMap.add (Variable.find l) lit v2l,
       aig
      ) (v2l,aig) latches
  in
  let aig =
    List.fold_left
      (fun aig (l,bdd) ->
       let aig,lit = add_bdd_to_aiger aig v2l bdd in
       let var = Variable.find l in
       Aiger.add_latch aig (VariableMap.find var v2l) lit (to_aiger_symbol l)
      ) aig latches 
  in 
  let aig =
    List.fold_left
      (fun aig (o,bdd) ->
       let aig,lit = add_bdd_to_aiger aig v2l bdd in
       let aig = Aiger.add_output aig lit (to_aiger_symbol o) in
       aig 
      ) aig outputs 
  in 
  aig 

exception Unsatisfiable of (Aiger.t * Cudd.bdd)
(*Aiger.t * bool VariableMap.t*)

let bdd_to_aiger ~inputs ~latches ~outputs ~wires bdd =
  let wire_cube = Variable.make_cube (List.map Variable.find wires) in
  let bdd = Cudd.bddExistAbstract bdd wire_cube in
  let next_latches = List.map (fun l -> Variable.next (Variable.find l)) latches in
  let cube = Variable.make_cube (List.rev_append next_latches (List.map Variable.find outputs)) in
  let rec aux (bdd,accu) (sym,var) = 
    (*let var = Variable.find latch in*)
    let value = Cudd.bddExistAbstract (Cudd.bddAnd (Variable.to_bdd var) bdd) cube in
    Cudd.bddAnd bdd
      (Cudd.bddOr 
	 (Cudd.bddAnd value (Variable.to_bdd var))
	 (Cudd.bddAnd (Cudd.bddNot value) (Cudd.bddNot (Variable.to_bdd var)))),
    (sym,value)::accu
  in
  
  let unsatisfiable = Cudd.bddNot (Cudd.bddExistAbstract bdd cube) in
  if Cudd.equal unsatisfiable (Cudd.bddFalse ())
  then 
    let bdd,update_function = List.fold_left aux (bdd,[]) (List.map (fun l -> l ,  Variable.next (Variable.find l)) latches) in
    let bdd,output_function = List.fold_left aux (bdd,[]) (List.map (fun o -> o , Variable.find o) outputs) in
    bdds_to_aiger inputs update_function output_function
  else
    (*let valuations = bdd_to_valuations unsatisfiable (List.rev_append (List.map Variable.find inputs) (List.map Variable.find latches)) in*)

    let _,update_function = List.fold_left aux (Cudd.bddTrue(),[]) (List.map (fun l -> l ,  Variable.next (Variable.find l)) latches) in
    let _,output_function = List.fold_left aux (Cudd.bddTrue(),[]) (List.map (fun o -> o , Variable.find o) outputs) in
    let aig = bdds_to_aiger inputs update_function output_function in
    raise (Unsatisfiable (aig,unsatisfiable(*List.hd valuations*)))


let variables_aiger aiger = 
  (* set of variables *)
  let vs = VariableSet.empty in

  let vs = 
    List.fold_left
      (fun vs inp -> 
       VariableSet.add (Variable.find (of_aiger_symbol (Aiger.lit2symbol aiger inp))) vs
      ) vs (List.rev_append aiger.Aiger.inputs aiger.Aiger.outputs) 
  in
  let vs = List.fold_left (fun vs (l,_) -> VariableSet.add (Variable.find (of_aiger_symbol (Aiger.lit2symbol aiger l))) vs) vs aiger.Aiger.latches in
  vs

let bdd_of_valuation valuation =
  VariableMap.fold 
    (fun var bool accu -> 
     if bool
     then Cudd.bddAnd accu (Variable.to_bdd var)
     else Cudd.bddAnd accu (Cudd.bddNot (Variable.to_bdd var))
    ) valuation (Cudd.bddTrue ())


(*
let bdd_of_inputs input_valuation =
  Aiger.LitMap.fold
    (fun lit b accu -> 
     Cudd.bddAnd accu (if b then bdd_of_variable (variable_of_input lit) else bdd_of_not_variable (variable_of_input lit))
    ) input_valuation (Cudd.bddTrue ())
 *)


exception Undefined of string 



(** compute updates in a table, all variables are even *)
let compute_updates aiger =
  let gate_bdd = Hashtbl.create aiger.Aiger.maxvar in
  let updates = Hashtbl.create aiger.Aiger.num_latches in
  Hashtbl.add gate_bdd Aiger.aiger_false (Cudd.bddFalse ());
  Hashtbl.add gate_bdd Aiger.aiger_true (Cudd.bddTrue ());

  List.iter 
    (fun inp -> 
     let var = Variable.find (of_aiger_symbol (Aiger.lit2symbol aiger inp)) in
     let bdd = Variable.to_bdd var in
     Hashtbl.add gate_bdd inp bdd;
     Hashtbl.add gate_bdd (Aiger.aiger_not inp) (Cudd.bddNot bdd)
    ) aiger.Aiger.inputs;

  List.iter 
    (fun (l,_) -> 
     let var = Variable.find (of_aiger_symbol (Aiger.lit2symbol aiger l)) in
     let bdd = Variable.to_bdd var in
      Hashtbl.add gate_bdd l bdd;
      Hashtbl.add gate_bdd (Aiger.aiger_not l) (Cudd.bddNot bdd)
    ) aiger.Aiger.latches;

  List.iter 
    (fun (g,l,r) -> 
      try
	let gl = Hashtbl.find gate_bdd l in
	let gr = Hashtbl.find gate_bdd r in
	let b = Cudd.bddAnd gl gr in
	Hashtbl.add gate_bdd g b;
	Hashtbl.add gate_bdd (Aiger.aiger_not g) (Cudd.bddNot b)
      with Not_found -> 
	Printf.eprintf "gate %d or %d not found\n" (Aiger.lit2int l) (Aiger.lit2int r);
	raise Not_found
    ) aiger.Aiger.ands;

  List.iter 
    (fun (l,n) -> 
      try 
	(*let bdd = Hashtbl.find gate_bdd n in
	Printf.eprintf "next latch %d (var %d) = gate %d\n" (Aiger.lit2int l) (Variable.find (of_aiger_symbol (Aiger.lit2symbol aiger l))) (Aiger.lit2int n);*)
	Hashtbl.add updates (Variable.find (of_aiger_symbol (Aiger.lit2symbol aiger l))) (Hashtbl.find gate_bdd n)
      with Not_found -> 
	Printf.eprintf "gate %d not found\n" (Aiger.lit2int n);
	raise Not_found 
    ) aiger.Aiger.latches;

  List.iter 
    (fun name -> 
     let litterals = Aiger.name_to_literals aiger name in
     Array.iteri 
       (fun i lit ->
	(* Warning: several output can have the same litteral *)
	try Hashtbl.add updates (Variable.find (name,i)) (Hashtbl.find gate_bdd lit)
	with Not_found  -> 
	  (*if i = 0 then 
	    try Hashtbl.add updates (Variable.find (name,None)) (Hashtbl.find gate_bdd lit)
	    with Not_found  -> *)
	  (*else raise Not_found*)
	  Printf.eprintf "gate %d not found\n" (Aiger.lit2int lit);
	  raise Not_found
       ) litterals
    ) (Aiger.outputs aiger);

  updates


let valuation_of_list list = 
  List.fold_left (fun accu (x,b) -> VariableMap.add x b accu) VariableMap.empty list

let rec fixpoint f x =
  let y = f x in if Cudd.equal y x then x else fixpoint f y



let reorder_aiger aiger =
  let ands = Hashtbl.create aiger.Aiger.num_ands in
  let mapping = Hashtbl.create aiger.Aiger.num_ands in
  List.iter (fun (a,b,c) -> Hashtbl.add ands a (a,b,c)) aiger.Aiger.ands;


  let add_mapping k v = 
    if Aiger.sign k 
    then Hashtbl.add mapping (Aiger.aiger_not k) (Aiger.aiger_not v)
    else Hashtbl.add mapping k v
  in
	     
  let find_mapping r = 
    let m = Hashtbl.find mapping (Aiger.strip r) in
    if Aiger.sign r then Aiger.aiger_not m else m
  in

  add_mapping Aiger.aiger_true Aiger.aiger_true;
  add_mapping Aiger.aiger_false Aiger.aiger_false;

  let max_lit = 0 in
  let max_lit = 
    List.fold_left 
      (fun ml a -> 
       add_mapping a (Aiger.int2lit (ml+2)); 
       ml + 2
      ) max_lit aiger.Aiger.inputs 
  in
  let max_lit = 
    List.fold_left 
      (fun ml (a,_) -> 
       add_mapping a (Aiger.int2lit (ml+2)); 
       ml + 2
      ) max_lit aiger.Aiger.latches
  in


  let rec explore gates max_lit a = 
    try gates, find_mapping a, max_lit
    with Not_found ->
      (*if (* Aiger.lit2int a < 2 * (aiger.Aiger.num_latches + aiger.Aiger.num_inputs + 1) *) 
	
      then gates, a, max_lit
      else*)
	let (lhs,rhs0,rhs1) = Hashtbl.find ands a in
	let gates,striped0,max_lit = explore gates max_lit (Aiger.strip rhs0) in 
	let new_rhs0 = if Aiger.sign rhs0 then Aiger.aiger_not striped0 else striped0 in
	let gates,striped1,max_lit = explore gates max_lit (Aiger.strip rhs1) in 
	let new_rhs1 = if Aiger.sign rhs1 then Aiger.aiger_not striped1 else striped1 in
	let gate = Aiger.int2lit (max_lit + 2) in
	add_mapping lhs gate;
	(gate,new_rhs0, new_rhs1)::gates, gate,  max_lit+2
  in

  let gates,max_lit = 
    List.fold_left
      (fun (accu,max_lit) (a,_,_) -> 
       let (a,_,m) = explore accu max_lit a in a,m
      ) (* ([],2 * (aiger.Aiger.num_latches +  aiger.Aiger.num_inputs)) aiger.Aiger.ands *)
      ([],max_lit) aiger.Aiger.ands 
  in

  let inputs = List.map find_mapping aiger.Aiger.inputs in

  let latches,gates,max_lit = 
    List.fold_left
      (fun (latches,gates,max_lit) (l,r) -> 
       let (g,nr,m) = explore gates max_lit r in
       (find_mapping l, nr) :: latches, g , m
      ) ([],gates,max_lit) aiger.Aiger.latches 
  in

  let outputs,gates,max_lit =  
    List.fold_left
      (fun (outputs,gates,max_lit) o ->
       let (g,no,m) = explore gates max_lit o in
       find_mapping o :: outputs, g, m
      ) ([],gates,max_lit) aiger.Aiger.outputs  
  in

  let symbols = Aiger.SymbolMap.map find_mapping aiger.Aiger.symbols in
  
  let abstract = 
    Aiger.LitMap.fold
      (fun l s accu -> 
       try Aiger.LitMap.add (find_mapping l) s accu
       with Not_found -> 
	 Printf.eprintf "Warning in AigerBdd.reorder_aiger: literal %d not found\n" (Aiger.lit2int l);
	 accu
      )  aiger.Aiger.abstract Aiger.LitMap.empty in

  {aiger with 
    inputs = inputs; 
    ands = List.rev gates; 
    latches = List.rev latches; outputs=outputs; 
    num_ands = List.length gates;
    num_latches = List.length latches;
    num_outputs =  List.length outputs;
    num_inputs =  List.length inputs;
    symbols=symbols; abstract=abstract; maxvar=max_lit/2}



module Circuit = 
  struct

type t = 
  {
    updates:(Variable.t , Cudd.bdd) Hashtbl.t;
    variables: VariableSet.t;
    next_variables: VariableSet.t;
    map: Aiger.lit VariableMap.t;
    array_variables: Variable.t array;
    array_next_variables: Variable.t array;
    composition_vector: Cudd.bdd array;
  }

let updates p = p.updates
let variables p = p.variables
let next_variables p = p.next_variables
let array_variables p = p.array_variables
let array_next_variables p = p.array_next_variables
let composition_vector p = p.composition_vector
let map p = p.map

let of_aiger aiger = 
  let updates = compute_updates aiger in
  let variables = variables_aiger aiger in
  let next_variables = VariableSet.fold (fun x accu -> VariableSet.add (Variable.next x) accu) variables VariableSet.empty in
  let array_variables = Array.of_list (VariableSet.elements variables) in
  let array_next_variables = Array.of_list (VariableSet.elements next_variables) in
  let composition_vector = 
    Array.init (* (aiger.Aiger.maxvar * 2 + 2) (* should it really be this value ? *)*)
      (Variable.max_var ())
	       (fun i -> 
		try Hashtbl.find updates (i-1)
		with Not_found -> Cudd.bddTrue())
  in
  let map = map_of_aiger aiger in
  { updates = updates; variables=variables; next_variables = next_variables;
    array_variables=array_variables; array_next_variables = array_next_variables;
    composition_vector=composition_vector; map=map}

let rename_configuration bdd variables next_variables =
  Cudd.bddSwapVariables bdd variables next_variables


let print_valuation aiger names valuation = 
  List.iter
    (fun name ->
      let size = Aiger.size_symbol aiger name in
      let value = ref 0 in
      for i = size - 1 downto 0 do
	(value := 2 * !value + 
		    (if VariableMap.find (Variable.find (name,i)) valuation
	    then 1 else 0));
	(*Printf.printf "%s.(%d) (= var %d): %b\n" name i (Variable.to_int (Variable.find (name,i))) (VariableMap.find (Variable.find (name,i)) valuation);*)
	
      done;
      Printf.printf "%s = %d\n" name !value
    ) names
    

let initial_state aiger = 
  valuation_of_list 
    (List.fold_left 
       (fun accu name -> 
	 let literals = Aiger.name_to_literals aiger name in
	 let variables = 
	   Array.mapi 
	     (fun i lit -> 
	       let v = Variable.find (name,i) 
	       in (v,false)
	     ) literals
	 in List.rev_append (Array.to_list variables) accu
       ) [] (List.rev_append (Aiger.latches aiger) (Aiger.outputs aiger))
    )




end
