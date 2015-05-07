type declaration =
| Input of AigerBdd.symbol 
| Output of AigerBdd.symbol 
| Reg of AigerBdd.symbol
| Wire of AigerBdd.symbol
| DList of declaration list


let rec add_declaration types declaration = match declaration with 
  | DList dl ->
    List.fold_left add_declaration types dl
  | Input i -> AigerBdd.SymbolMap.add i (Input i) types
  | Output o -> AigerBdd.SymbolMap.add o (Output o) types
  | Reg r -> AigerBdd.SymbolMap.add r (Reg r) types
  | Wire w -> AigerBdd.SymbolMap.add w (Wire w) types

let of_declaration dl =
  List.fold_left add_declaration AigerBdd.SymbolMap.empty dl

(*
let find_declaration dl name =
  let rec aux dl = match dl with 
    | [] -> None
    | hd :: tl -> match hd with
      | Input (x,i) | Output (x,i) | Reg (x,i) | Wire (x,i) when x = name -> Some hd
      | DList dl -> (match aux dl with
	| Some x -> Some x | None -> aux tl)
      | _ -> aux tl
  in aux dl

let size_declaration decl = match decl with
  | Input (_,i) | Output (_,i) | Reg (_,i) | Wire (_,i) -> i 
  | _ -> None
*)

let inputs types = 
  AigerBdd.SymbolMap.fold 
    (fun _ declaration accu ->
      match declaration with 
      | Input i -> i :: accu
      | _ -> accu
    ) types []

let outputs types = 
  AigerBdd.SymbolMap.fold 
    (fun _ declaration accu ->
      match declaration with 
      | Output i -> i :: accu
      | _ -> accu
    ) types []


let registers types = 
  AigerBdd.SymbolMap.fold 
    (fun _ declaration accu ->
      match declaration with 
      | Reg i -> i :: accu
      | _ -> accu
    ) types []

let wires types = 
  AigerBdd.SymbolMap.fold 
    (fun _ declaration accu ->
      match declaration with 
      | Wire i -> i :: accu
      | _ -> accu
    ) types []

let make_vector f name i = 
  let rec aux accu k = 
    if k < 0 then accu
    else aux (f (name,k) :: accu) (k-1)
  in aux [] (i-1)


let iter start last f x = 
  let rec aux i accu = 
    if i > last then accu else aux (i+1) (f i accu)
  in aux start x


let var = Integer.var

let input name size = 
  DList (make_vector (fun x -> Input x) name size),
  var name size
    
let output name size = 
  DList (make_vector (fun x -> Output x) name size),
  var name size

let reg name size = 
  DList (make_vector (fun x -> Reg x) name size),
  var name size

let wire name size = 
  DList (make_vector (fun x -> Wire x) name size),
  var name size



module Constraint =
struct
  type t = Cudd.bdd
  let of_bdd x = x
  let to_bdd x = x
 
  let of_list cl = 
    List.fold_left (fun accu x -> Cudd.bddAnd accu x) (Cudd.bddTrue()) cl

  let rec of_expr expr = match expr with
    | Boolean.EVar (x,i) -> AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (x,i))
    | Boolean.ENext (x,i) -> AigerBdd.Variable.to_bdd (AigerBdd.Variable.next (AigerBdd.Variable.find (x,i)))

    | Boolean.EForall (vl,e) -> 
      let variables = 
	List.fold_left
	  (fun accu e -> 
	   match e with 
	   | Boolean.EVar (x,i) -> AigerBdd.Variable.find (x,i) :: accu
	   | _ -> failwith "In Speculog.Constraint.of_expr: universal quantification on expressions that are not variables"
	  ) [] vl 
      in
      let cube = AigerBdd.Variable.make_cube variables in
      Cudd.bddUnivAbstract (of_expr e) cube

    | Boolean.EExists (vl,e) -> 
    let variables = 
      List.fold_left
	(fun accu e -> 
	 match e with 
	 | Boolean.EVar (x,i) -> AigerBdd.Variable.find (x,i) :: accu
	 | _ -> failwith "In Speculog.Constraint.of_expr: existential quantification on expressions that are not variables"
	) [] vl 
    in
    let cube = AigerBdd.Variable.make_cube variables in
    Cudd.bddExistAbstract (of_expr e) cube

    | Boolean.ENot e -> Cudd.bddNot (of_expr e)
    | Boolean.EAnd (hd :: tl) -> List.fold_left Cudd.bddAnd (of_expr hd) (List.map of_expr tl)
    | Boolean.EAnd [] -> Cudd.bddTrue()
    | Boolean.EOr (hd :: tl) ->  List.fold_left Cudd.bddOr (of_expr hd) (List.map of_expr tl)
    | Boolean.EOr [] -> Cudd.bddFalse()
(*    | Boolean.EXor (e,f) ->  Cudd.bddNot (Cudd.bddOr (Cudd.bddAnd (of_expr e) (of_expr f)) (Cudd.bddAnd (Cudd.bddNot (of_expr e)) (Cudd.bddNot (of_expr f))))
    | Boolean.EEqual (e,f) -> of_expr (Boolean.ENot (Boolean.EXor (e,f)))*)
    | Boolean.EList el -> of_list (List.map of_expr el)
    | Boolean.True -> Cudd.bddTrue()
    | Boolean.False -> Cudd.bddFalse()

  let expr_to_bdd e = to_bdd (of_expr e)


  let of_exprs el = 
    of_list (List.map of_expr el)

  let for_each start last f =
    iter start last (fun i c -> Cudd.bddAnd (f i) c) (Cudd.bddTrue())

end


exception NonSynthesizable of (bool Integer.t * bool Integer.t)
let synthesize declarations constr =
  let types = of_declaration declarations in
  let bdd = Constraint.expr_to_bdd (Integer.to_boolean constr) in
  try
    AigerBdd.bdd_to_aiger (inputs types) (registers types) (outputs types) (wires types) bdd
  with (AigerBdd.Unsatisfiable (aig,bdd)) -> 
    let expr = Boolean.of_bdd bdd (List.rev_append (inputs types) (registers types)) in
    (*AigerBdd.print_valuation aig (List.rev_append (Aiger.inputs aig) (Aiger.latches aig)) x;*)
    raise (NonSynthesizable (constr,Integer.make None [|expr|]))

let add_synthesized declarations constraints aiger =
  let inputs = Aiger.inputs aiger in
  let outputs = Aiger.outputs aiger in
  let latches = Aiger.latches aiger in  
  let decl = declarations in
  let decl = 
    List.fold_left 
      (fun decl n -> 
	let d, _ = input n (Aiger.size_symbol aiger n) in
	d :: decl
      ) decl inputs
  in
  let decl = 
    List.fold_left 
      (fun decl n -> 
	let d, _ = reg n (Aiger.size_symbol aiger n) in
	d :: decl
      ) decl latches
  in
  let decl = 
    List.fold_left 
      (fun decl n -> 
	let d, _ = output n (Aiger.size_symbol aiger n) in
	d :: decl
      ) decl outputs
  in
  synthesize decl constraints
      
let constraint_synthesis declarations = function 
  | hd :: tl -> 
     List.fold_left 
       (fun a b -> 
	add_synthesized declarations b a
       ) (synthesize declarations hd) tl
  | _ -> failwith "In Speculog.constraint_synthesis: no constraint given"
       
module SymbolSet = AigerBdd.SymbolSet

let names_in_boolean_expr expr = 
  let rec aux accu = function 
    | Boolean.EVar (name,i) | Boolean.ENext (name,i) -> SymbolSet.add (name,i) accu 
								      
    | Boolean.EExists (tl,e) 
    | Boolean.EForall(tl,e) -> (* Variables in tl should be removed *)
       List.fold_left
	 (fun accu -> function 
		   | Boolean.EVar (name,i) -> SymbolSet.remove (name,i) accu
		   | _ -> failwith "In Speculog.names_in_boolean_expr: quantification over an expression that is not a variables"
	 ) (aux accu e) tl
    | Boolean.ENot t -> aux accu t
    | Boolean.EAnd tl | Boolean.EOr tl 
    | Boolean.EList tl -> List.fold_left aux accu tl
    | Boolean.True | Boolean.False -> accu
  in aux SymbolSet.empty expr

let names_in_expr expr = 
  let arr = Integer.to_boolean_array expr in
  Array.fold_left (fun accu i -> SymbolSet.union accu (names_in_boolean_expr i)) SymbolSet.empty arr

let types_of_updates updates = 
  let read,write =
    List.fold_left 
      (fun (read,write) (var,expr) ->
       SymbolSet.union (names_in_expr expr) read,
       SymbolSet.union (names_in_expr var) write
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
(* (* Debug *)
  print_endline "registers:";
  SymbolSet.iter (fun (s,i) -> Printf.printf "%s<%d> ; " s i) latches;
  print_newline ();
  print_endline "inputs:";
  SymbolSet.iter (fun (s,i) -> Printf.printf "%s<%d> ; " s i) inputs;
  print_newline ();*)
  inputs,outputs,latches

(* The declarations are unecessary, we could deduce them from the expressions *)
let functional_synthesis (*declarations*) updates = 
  (*let types = of_declaration declarations in*)
  let inputs,outputs,latches = types_of_updates updates in
  let latches_bdds,outputs_bdds = 
    List.fold_left
      (fun (lb,ob) (var,expr) -> 
	let ba_var = Integer.to_boolean_array var in
	let lb,ob,nb = 
	  Array.fold_left 
	    (fun (lb,ob,i) b_var ->
	      match b_var with 
	      | Boolean.EVar (s,io) -> 
		 if SymbolSet.mem (s,io) latches
		 then (((s,io),Constraint.of_expr (Integer.get expr i))::lb,ob,i+1)
		 else if SymbolSet.mem (s,io) outputs
		 then (lb,((s,io),Constraint.of_expr (Integer.get expr i))::ob,i+1)
		 else failwith ("In Speculog.functional_synthesis: the variable "^s^" is neither a latch nor an output")
	      | _ -> failwith "In Speculog.functional_synthesis: the expression on the left should be a variable"
	    ) (lb,ob,0) ba_var 
	in lb,ob
      ) ([],[]) updates 
  in
  AigerBdd.bdds_to_aiger (SymbolSet.elements inputs) latches_bdds outputs_bdds 
    
let _declarations = []

let print_aiger a = Aiger.write a stdout
