(** This module defines usefull function for going from the aiger representation 
 * to the BDD representation and reciproqually *)
module Aiger = AigerImperative

let init aiger =
  try Cudd.init (aiger.Aiger.num_inputs+2*aiger.Aiger.num_latches+2)
  with Failure x -> Printf.eprintf "warning: %s\n" x

module Variable = BddVariable
module SymbolMap = Map.Make(struct type t = string let compare = compare end)
module SymbolSet = Set.Make(struct type t = string let compare = compare end)
module VariableMap = Map.Make(Variable)


let map_of_aiger aiger = 
  let m =
    List.fold_left
      (fun m inp -> 
	match Aiger.lit2string aiger inp with
	 | Some sym -> VariableMap.add (Variable.find sym) inp m
	 | None ->
	    if inp = 1 || inp = 0
	    then VariableMap.add (Variable.of_int (inp)) inp m
	    else failwith ("AigerImpBdd.map_of_aiger: wrong input, lit: "^string_of_int inp)
      ) VariableMap.empty (Aiger.LitSet.elements aiger.Aiger.inputs)
  in 
  Hashtbl.fold 
    (fun l _ m -> 
      match Aiger.lit2string aiger l with
      | Some sym -> VariableMap.add (Variable.find sym) l m
      | None -> failwith ("AigerImpBdd.map_of_aiger: wrong latch, lit: "^string_of_int l)
    ) aiger.Aiger.latches m 


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
      accu^Variable.to_string var^" -> "^string_of_int (Hashtbl.hash lit)^"; "
    ) map ""

exception UndeclaredLit of Aiger.lit

module BddMap = Map.Make(struct type t = Cudd.bdd let compare = Cudd.compare end)

(* We should normalize the cache: ie no negated nodes *)
let add_bdd_to_aiger aig v2l bdd = 
  let v2l = VariableMap.merge 
    (fun k a b -> match a,b with 
    | Some x , _ | None , Some x -> Some x
    | _ -> None
    ) v2l (map_of_aiger aig) in

  let cache = BddMap.empty in

  let rec aux bdd cache = 
    try (BddMap.find bdd cache,cache)
    with Not_found ->
      if Cudd.isConstant bdd
      then
	if Cudd.value bdd = 1
	then (Aiger.aiger_true,cache)
	else (Aiger.aiger_false,cache)
      else
	let variable = Cudd.nodeReadIndex bdd in
	let lit = 
	  try VariableMap.find (BddVariable.of_int variable) v2l 
	  with Not_found -> 
	    failwith ("In AigerBdd.add_bdd_to_aiger: variable "^string_of_int variable^" not found in the given Aiger.lit VariableMap.t")
	in
	let then_child = Cudd.t bdd in
	let else_child = Cudd.e bdd in
	let (then_lit,cache) = aux then_child cache in
	let (else_lit,cache) = aux else_child cache in

	let lit1 = Aiger.conj aig lit then_lit in
	let lit2 = Aiger.conj aig (Aiger.neg lit) else_lit in
	let lit3 = Aiger.conj aig (Aiger.neg lit1) (Aiger.neg lit2) in
	
	let res = 
	  if Cudd.isComplement bdd then lit3 else Aiger.neg lit3
	in
	(res,BddMap.add bdd res cache)
  in 
  let res,_ = aux bdd cache in
  res

  
let bdds_to_aiger ~inputs ~latches ~outputs =
  (* mapping variable to litterals *)
  let aig = Aiger.empty () in
  let v2l = VariableMap.empty in
  let v2l = 
    List.fold_left
      (fun v2l i -> 
	let lit = Aiger.add_input aig i in
	VariableMap.add (Variable.find i) lit v2l
      ) v2l inputs
  in

  let v2l = 
    List.fold_left
      (fun v2l (l,_) -> 
	let lit = Aiger.add_latch aig l in
	VariableMap.add (Variable.find l) lit v2l
      ) v2l latches
  in

  List.iter
    (fun (l,bdd) ->
       let lit = add_bdd_to_aiger aig v2l bdd in
       let var = Variable.find l in
       Aiger.set_latch_update aig (VariableMap.find var v2l) lit 
    ) latches ;

  List.iter
    (fun (o,bdd) ->
      let lit = add_bdd_to_aiger aig v2l bdd in
      Aiger.set_output aig o lit
    ) outputs;

  aig 


exception Unsatisfiable of (Aiger.t * Cudd.bdd)

(*Aiger.t * bool VariableMap.t*)

(*
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
*)

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


let rec fixpoint f x =
  let y = f x in if Cudd.equal y x then x else fixpoint f y

let valuation_of_list list = 
  List.fold_left (fun accu (x,b) -> VariableMap.add x b accu) VariableMap.empty list


let reorder_aiger aiger =
  failwith "AigerBdd.reorder_aiger: not implemented" (*
  let ands = Hashtbl.create aiger.Aiger.num_ands in
  let mapping = Hashtbl.create aiger.Aiger.num_ands in
  Hashtbl.iter (fun a (b,c) -> Hashtbl.add ands a (a,b,c)) aiger.Aiger.ands;


  let add_mapping k v = 
    if Aiger.sign k 
    then Hashtbl.add mapping (Aiger.neg k) (Aiger.neg v)
    else Hashtbl.add mapping k v
  in
	     
  let find_mapping r = 
    let m = Hashtbl.find mapping (Aiger.strip r) in
    if Aiger.sign r then Aiger.neg m else m
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
	let new_rhs0 = if Aiger.sign rhs0 then Aiger.neg striped0 else striped0 in
	let gates,striped1,max_lit = explore gates max_lit (Aiger.strip rhs1) in 
	let new_rhs1 = if Aiger.sign rhs1 then Aiger.neg striped1 else striped1 in
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
						     *)

