exception NonSynthesizable of (Boolean.t * Boolean.t)
(*let synthesize declarations expr =
  let types = of_declaration declarations in
  let bdd = expr_to_bdd expr in
  try
    AigerBdd.bdd_to_aiger (inputs types) (registers types) (outputs types) (wires types) bdd
  with (AigerBdd.Unsatisfiable (aig,bdd)) -> 
    let expr_b = Boolean.of_bdd bdd (List.rev_append (inputs types) (registers types)) in
    raise (NonSynthesizable (expr,expr_b))

let add_synthesized declarations constraints aiger =
  failwith "SynthesisImp.add_synthesized: unimplemented"
*)
  
module SymbolSet = AigerBdd.SymbolSet

let names_in_boolean_expr expr = 
  let rec aux accu = function 
    | Boolean.EVar name -> SymbolSet.add name accu 
    | Boolean.ENot t -> aux accu t
    | Boolean.EAnd tl | Boolean.EOr tl 
    | Boolean.EList tl -> List.fold_left aux accu tl
    | Boolean.True | Boolean.False -> accu
  in aux SymbolSet.empty expr

let names_in_expr expr = 
  let arr = Integer.to_boolean_array expr in
  Array.fold_left (fun accu i -> 
    SymbolSet.union accu (names_in_boolean_expr i)) SymbolSet.empty arr

let types_of_updates updates = 
  let read,write =
    List.fold_left 
      (fun (read,write) (var,expr) ->
	let to_read = names_in_expr expr in
	let to_write = names_in_expr var in
	SymbolSet.union to_read read, SymbolSet.union to_write write
      ) (SymbolSet.empty,SymbolSet.empty) updates     
  in
  let latches,inputs = 
    SymbolSet.fold 
      (fun s (l,i) -> 
       if SymbolSet.mem s write then SymbolSet.add s l,i else l,SymbolSet.add s i
      ) read (SymbolSet.empty,SymbolSet.empty)
  in
  let outputs = 
    SymbolSet.fold
      (fun s o -> 
       if SymbolSet.mem s read then o else SymbolSet.add s o
      )  write SymbolSet.empty
  in
  inputs,outputs,latches

let functional_synthesis updates = 
  let inputs,outputs,latches = types_of_updates updates in
  let latches_bdds,outputs_bdds = 
    List.fold_left
      (fun (lb,ob) (var,expr) -> 
	let ba_var = Integer.to_boolean_array var in
	let lb,ob,nb = 
	  Array.fold_left 
	    (fun (lb,ob,i) b_var ->
	      match b_var with 
	      | Boolean.EVar s -> 
		if SymbolSet.mem s latches
		then ((s,Integer.get expr i)::lb,ob,i+1)
		else if SymbolSet.mem s outputs
		then (lb,(s,(Integer.get expr i))::ob,i+1)
		else failwith ("In SynthesisImp.functional_synthesis: the variable "^s^" is neither a latch nor an output")
	      | _ -> failwith "In SynthesisImp.functional_synthesis: the expression on the left should be a variable"
	    ) (lb,ob,0) ba_var 
	in lb,ob
      ) ([],[]) updates 
  in
  AigerImpBdd.bdds_to_aiger (SymbolSet.elements inputs) 
    (List.map (fun (string,boolean) -> string, Boolean.to_bdd boolean) latches_bdds)
    (List.map (fun (string,boolean) -> string, Boolean.to_bdd boolean) outputs_bdds)
    
