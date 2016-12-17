(* let () = Common.display_debug := true *)

type parameter = Int of int | String of string | Bool of bool

type t = (string -> parameter) -> Aiger.t

exception WrongParameter of parameter 

let get_int = function Int i -> i | x -> raise (WrongParameter x)
let get_string = function String s -> s | x -> raise (WrongParameter x)
let get_bool = function Bool b -> b | x -> raise (WrongParameter x)

(* The program is executed only if the executable name correspond to the parameter *)
let make exec_name parameters f = 
  if Filename.basename Sys.argv.(0) = exec_name
  then
    let table = Hashtbl.create (List.length parameters) in
    let add p v = Hashtbl.replace table p v in
    let find p = Hashtbl.find table p in
    let specs = 
      List.map (fun (name,p) -> 
      (*add name p; *)
	match p with
	| Int i -> 
	  add name (Int i);
	  "-"^name,Arg.Int (fun j -> add name (Int j)), "integer parameter "^name
	| String s -> 
	  add name (String s);
	  "-"^name,Arg.String (fun j -> add name (String j)), "string parameter "^name
	| Bool b -> 
	  add name (Bool b);
	  "-"^name,Arg.Bool (fun j -> add name (Bool j)), "boolean parameter "^name
      ) parameters
    in
    Arg.parse specs ignore "This is a parametric module";
    let m = f find in
    Aiger.write m stdout
    
type link = 
| True | False | Symb of Aiger.Symbol.t | Not of Aiger.Symbol.t

let link_to_string = function
  | True -> "true"
  | False -> "false"
  | Symb s -> Aiger.Symbol.to_string s
  | Not s -> "!"^Aiger.Symbol.to_string s

type declaration =
| Wire of Aiger.Symbol.t
| Input of Aiger.Symbol.t 
| Output of Aiger.Symbol.t 
| Reg of Aiger.Symbol.t
| UpdateReg of Aiger.Symbol.t * link
| EqualAnd of Aiger.Symbol.t * link  * link 
| UseModule of Aiger.t * (Aiger.Symbol.t * Aiger.Symbol.t) list
| UseParametricModule of t * (string * parameter) list * (Aiger.Symbol.t * Aiger.Symbol.t) list
										       

let find_sym aig map b = 
  try (*Aiger.var2lit*) (Aiger.SymbolMap.find b map)
  with Not_found -> 
    try Aiger.symbol2lit aig b
    with Not_found ->
      match b with 
      | name,None ->
	(try Aiger.symbol2lit aig (name,Some 0)
	 with Not_found ->
	   try Aiger.SymbolMap.find (name,Some 0) map
	   with Not_found ->
	     Printf.printf "B Warning: Symbol %s not found\n" (Aiger.Symbol.to_string b);
	     raise Not_found)
      | name,Some 0 ->
	(try Aiger.symbol2lit aig (name,None)
	 with Not_found ->
	   try Aiger.SymbolMap.find (name,None) map
	   with Not_found ->
	     Printf.printf "A Warning: Symbol %s not found\n" (Aiger.Symbol.to_string b);
	     raise Not_found)


(* Given a map and a name, give a list containing pair of literal and symbols *)
let find_all_sym map b = 
  Aiger.SymbolMap.fold (fun k var l -> match k with 
  | name, x when name = b -> ((*Aiger.var2lit*) var,(name,x)) :: l
  | _ -> l) map []

let find_link aig map = function
  | True -> Aiger.aiger_true 
  | False -> Aiger.aiger_false
  | Symb b -> find_sym aig map b
  | Not b -> Aiger.aiger_not (find_sym aig map b)
    
let make_output aig lit sym = 
  if List.mem lit aig.Aiger.outputs then aig
  else Aiger.add_output aig lit sym


(* given links add an and gate *)
let add_and (symbols, outputs, accu) a b c = 
  if !Common.display_debug
  then Printf.printf "add and %s %s %s\n" (Aiger.Symbol.to_string a) (link_to_string b) (link_to_string c) ;

  let lb = find_link accu symbols b in 
  let lc = find_link accu symbols c in
  let add_new_lit = Aiger.lit2int lb > 1 && Aiger.lit2int lc > 1 in

  let aig,map,la =
    if Aiger.SymbolMap.mem a outputs
    then 
      let () = Common.debug "mem outputs" in
      let aig,va = Aiger.new_var accu in
      let map = Aiger.SymbolMap.add a (Aiger.var2lit va) symbols in
      (*let aig = Aiger.add_output aig (Aiger.var2lit va) a in*)
      let la = Aiger.var2lit va in
      let aig = make_output aig (Aiger.var2lit va) a in
      let aig = Aiger.add_and aig la lb lc in
      aig,map,la
    else
      let () = Common.debug "not mem outputs" in
      if Aiger.SymbolMap.mem a symbols 
      then 
	let () = Printf.eprintf  "Warning: variable %s already present in symbols\n" (Aiger.Symbol.to_string a) in
	let la = find_sym accu symbols a in
	let aig = Aiger.add_and accu la lb lc in
	aig,symbols,la
      else
	let () = Common.debug "not mem symbols" in
	(* if the variable as not been declared yet we add it *)
	(* the following is very similar to the output case *)
	(* !!!! there is a problem if b or c is an output *)

	if lb = Aiger.aiger_true 
	then 
	  let map = Aiger.SymbolMap.add a lc symbols in
	  accu,map,lc
	else if lc = Aiger.aiger_true 
	then
	  let map = Aiger.SymbolMap.add a lb symbols in
	  accu,map,lb
	else if lc = Aiger.aiger_false || lb = Aiger.aiger_false
	then 
	  let map = Aiger.SymbolMap.add a Aiger.aiger_false symbols in
	  accu,map,Aiger.aiger_false
	else if lb = lc 
	then 
	  let map = Aiger.SymbolMap.add a lb symbols in
	  accu,map,lb
	else
	  let aig,va = Aiger.new_var accu in
	  let la = Aiger.var2lit va in
	  let map = Aiger.SymbolMap.add a la symbols in
	  let aig = Aiger.add_and aig la lb lc in
	  aig,map,la
  
  in
    
  if (Aiger.lit2int la)/2 < (Aiger.lit2int lb)/2 || (Aiger.lit2int la)/2 < (Aiger.lit2int lc)/2
  then Printf.printf "warning: add and has input literals greater than the output: %d %d %d\n***********\n*******\n" (Aiger.lit2int la) (Aiger.lit2int lb) (Aiger.lit2int lc);

  map, outputs,aig

(* generate a list from 0 to i-1 *)
let gen_list i = 
  let rec aux accu j =
    if j < 0 then accu else aux (j::accu) (j-1)
  in aux [] (i-1)

(* size of a symbol *)
let size_symbol map aig sym = 
  let a_size = Aiger.size_symbol aig sym in
  if a_size = 0 then 
    if Aiger.SymbolMap.mem (sym,None) map 
    (* I think the case of outputs should also be treated *)
    then 1
    else 
      let (nb,maxi) = Aiger.SymbolMap.fold (fun (s,io) l (nb,maxi) -> 
	let i = match io with None -> 0 | Some x -> x in
	if s = sym then (nb+1,max maxi i) else (nb,maxi)) map (0,-1) in
      if maxi + 1 <> nb
      then
	(Printf.eprintf "warning: %d symbols to encode %s but the maximum index used is %d\n" nb sym maxi;
	 max nb maxi)
      else nb

  else a_size
    

let symbol2string sym = match sym with
  | name,None -> name
  | name, Some i -> name^" [< "^string_of_int i^" >] "

(* use the module [mo] inside the declaration of [aig] *)
let use_module (map,out, aig) mo renaming = 
  Common.debug "use module";
  (* Here are the steps we need to do
     1) the inputs of [mo] are added as outputs of [aig];
     2) we rename inputs of [mo] to correspond to these outputs;
     3) we compose the modules;
     4) we link the outputs of [mo] to the corresponding literal of [aig];
     5) we hide the outputs of [mo];
     6) we also hide the outputs we have added in step 1)
  *)
  
  (* we first look what are the inputs of [mo] *)
  (* looking just at there name may not be enough *)
  let inputs = Aiger.inputs mo in
  let latches = Aiger.latches mo in
  let outputs = Aiger.outputs mo in
  let is_input name = List.mem name inputs in
  let is_output name = List.mem name outputs in
  let is_latch name = List.mem name latches in


  (* make the renaming explicit *)
  (* Some stuff we do in the next functions might be unecessary now *)
  let renaming = List.fold_left (fun r (s,t) -> 
      match (s,t) with
    | ((a,Some c) , (b,Some d)) -> ((a,Some c),(b,Some d)) :: r
      
    | ((a,None),(b,None)) -> 
      let size_a = Aiger.size_symbol mo a in
      let size_b = size_symbol map aig b in
      
      if size_a <> size_b && !Common.display_debug
      then Printf.eprintf "warning: %s and %s do not have the same size (%d/%d)\nthis is normal if %s is a wire\n" a b size_a size_b b;
      
      let mapping = 
	if size_b = 1
	then
	  if size_a = 1 
	  then [(a,None),(b,None)]
	  else [(a,Some 0),(b,None)]
	else
	  if size_b = 0
	  then 
	    if size_a = 1 
	    then [(a,None),(b,None)]
	    else
	      List.map (fun i -> (a,Some i),(b,Some i)) (gen_list size_a)
	  else
	    List.map (fun i -> (a,Some i),(b,Some i)) (gen_list (min size_a size_b))
      in
      List.rev_append mapping r

    | ((a,Some c) , (b,None)) -> 
      if size_symbol map aig b  <> 1
      then 
	(Printf.eprintf "Warning: %s as not size 1\nTaking only the first bit\n" b;
	 ((a,Some c) , (b,Some 0)) :: r)
      else
	((a,Some c) , (b,None)) :: r

    | ((a,None) , (b,Some d)) ->
	if Aiger.size_symbol mo a <> 1 
	then 
	  (Printf.eprintf "Warning: %s is to small for %s\n" b a;
	   ((a,Some 0),(b,Some d)) :: r)
	else ((a,None),(b,Some d)) :: r 
	  
  ) [] renaming
  in


  if !Common.display_debug 
  then 
    (
      print_endline "renaming: <";
      List.iter (fun (x,y) -> Printf.printf "%s -> %s\n" (symbol2string x) (symbol2string y)) renaming;
      print_endline ">");

  
  let input_renaming = List.filter (fun x -> is_input (fst (fst x))) renaming in
  let latch_renaming = List.filter (fun x -> is_latch (fst (fst x))) renaming in
  let inputs,outputs = 
    List.fold_left (fun (inputs,outputs) x -> 
      if is_input (fst (fst x)) 
      then (snd x) :: inputs , outputs 
      else if is_output (fst (fst x)) then inputs, (snd x) :: outputs 
      else inputs, outputs
    ) ([],[]) renaming
  in
  (* we make variables corresponding to inputs of [mo] into outputs of [aig] *)
  Common.debug "making outputs";

  let outs = (Aiger.outputs aig) in
  let aig,added_outputs = List.fold_left 
    (fun (aig,ao) sym -> 
      (* we should may be not look only at the name *)
      try
	if List.mem (fst sym) outs
	then aig,ao
	else 
	  (  
	     let lit = find_sym aig map sym in
	     make_output aig lit sym, sym :: ao
	  )
      with Not_found -> 
	Printf.printf "Error: symbol %s should have been instanciated before being used as input of a module\n" (symbol2string sym);
	print_endline "current symbols :";
	Aiger.SymbolMap.iter (fun k l -> 
	  print_endline (symbol2string k)) aig.symbols;
	raise Not_found
    ) (aig,[]) inputs 
  in

  if !Common.display_debug then (
    print_endline "OUTPUTED AIGER >>>";
    Aiger.write aig stdout;
    print_endline "<<<<<<<<<";);
 

  (* we rename outputs of [mo] to correspond to outputs of [aig] *)
  Common.debug "renaming";
  let renamed = Aiger.rename mo (List.rev_append latch_renaming input_renaming) in

  (* we compose the modules *)
  Common.debug "composing";
  let aig = Aiger.compose aig renamed in


  if !Common.display_debug 
  then 
    (print_endline "COMPOSED AIGER >>>";
     Aiger.write aig stdout;
     print_endline "<<<<<<<<<";);

  (* we link the symbols with the outputs of [mo] *)
  Common.debug "linking";
  let assoc2 sym = 
    fst (List.find (fun x -> snd x = sym ) renaming)
  in

  let (map,out,aig) = List.fold_left 
    (fun (map,out,aig) sym ->
      try
	let equiv = assoc2 sym in
	add_and (map,out,aig) sym (Symb equiv) True
      with Not_found -> Printf.printf "renaming of %s not found\n" (symbol2string sym);
	raise Not_found
    ) (map,out,aig) outputs in


  if !Common.display_debug then (
    print_endline "LINKED AIGER >>>";
    Aiger.write aig stdout;
    print_endline "<<<<<<<<<";);

  (* we now hide outputs of [mo] *)
  let aig = List.fold_left (fun aig sym -> 
    let aig_sym = assoc2 sym in
    if !Common.display_debug then Printf.printf "hiding %s\n" (Aiger.Symbol.to_string aig_sym);
    Aiger.hide aig aig_sym
  ) aig outputs in

  (* outputs that have been added should return to there normal state *)
  (*if !Common.display_debug then 
    List.iter (fun x -> Printf.printf "hiding %s\n" (Aiger.Symbol.to_string x)) added_outputs;
   *)

  let aig = List.fold_left Aiger.hide aig added_outputs in

(*
  if !Common.display_debug 
  then (  
    Printf.printf "\nSymbols:\n";
    Aiger.SymbolMap.iter (fun k a -> 
      Printf.printf "%s " (Aiger.Symbol.to_string k)) map;
    Printf.printf "\nOutputs:\n";
    Aiger.SymbolMap.iter (fun k a -> 
      Printf.printf "%s " (Aiger.Symbol.to_string k)) out;
    print_newline ();
    print_endline "AIGER >>>";
    Aiger.write aig stdout;
    print_endline "<<<<<<<<<");
 *)
  map,out,aig

exception BadDeclaration

let add_declaration (symbols,outputs,accu) d =
  try match d with 
  | Wire x -> 
    Printf.eprintf "Warning: there no need to declare wires\n";
    (* let aig,va = Aiger.new_var accu in
    let map = Aiger.SymbolMap.add x va symbols in map,outputs,aig*)
    (symbols,outputs,accu)

  | Input i ->
    let aig,va = Aiger.new_var accu in
    let map = Aiger.SymbolMap.add i (Aiger.var2lit va) symbols in
    let aig = Aiger.add_input aig (Aiger.var2lit va) i in
    (map,outputs,aig)

  | Output o ->
    (symbols,Aiger.SymbolMap.add o true outputs, accu)

  | EqualAnd (a,b,c) -> add_and (symbols,outputs,accu) a b c

  | Reg r -> 
    let aig,vr = Aiger.new_var accu in
    let map = Aiger.SymbolMap.add r (Aiger.var2lit vr) symbols in
    map, outputs, aig


  | UpdateReg ((n,Some i),l) ->
    let ll = find_link accu symbols l in 
    let vr = Aiger.SymbolMap.find (n,Some i) symbols in
    let aig = Aiger.add_latch accu vr ll (n,Some i) in
    symbols, outputs, aig

  | UpdateReg ((r,None),l) -> (* Symb l) ->*)
    let size_r = size_symbol symbols accu r in
    if size_r = 1 
    then
      let ll = find_link accu symbols l (*(Symb l)*) in 
      let vr = Aiger.SymbolMap.find (r,None) symbols in
      let aig = Aiger.add_latch accu vr ll (r,None) in
      symbols, outputs, aig
    else 
      (match l with 
       | Symb l ->
	  let aig = 
	    List.fold_left 
	      (fun aig i ->
		let ll = find_link accu symbols (Symb (fst l,Some i)) in
		let vr = Aiger.SymbolMap.find (r,Some i) symbols in
		Aiger.add_latch aig ((*Aiger.var2lit *)vr) ll (r,Some i)
	      ) accu (gen_list size_r)
	  in  
	  symbols, outputs, aig
       | _ -> 
	  Printf.printf "cannot assign register %s of size %d to Boolean value\n" r size_r;
	  raise BadDeclaration
      )
	


  | UseModule (mo,renaming) -> use_module (symbols,outputs,accu) mo renaming
  | UseParametricModule (pm,parameters,renaming) -> use_module (symbols,outputs,accu) (pm (fun x -> List.assoc x parameters)) renaming

  with Not_found -> 
    Printf.printf "One variable or parameter was not declared\n";
    print_endline "Current state of the generated circuit:";
    Aiger.write accu stdout;
    raise Not_found
  | x -> 
    Printf.printf "Unknown error\n";
    print_endline "Current state of the generated circuit:";
    Aiger.write accu stdout;
    raise x

let of_declaration declarations = 
  let map,outputs,aig = List.fold_left add_declaration (Aiger.SymbolMap.empty,Aiger.SymbolMap.empty,Aiger.empty) declarations in
  aig

let make_vector f name i j = 
  let rec aux accu k = 
    if k < j then accu
    else aux (f (name,Some k) :: accu) (k-1)
  in aux [] i

let vector_input = make_vector (fun x -> Input x)
let vector_output = make_vector (fun x -> Output x)
let vector_wire = make_vector (fun x -> Wire x)
let vector_reg = make_vector (fun x -> Reg x)

(*
let vector_reg name1 name2 i j = 
  let rec aux accu k = 
    if k < j then accu
    else aux (Reg((name1,Some k),Symb(name2,Some k)) :: accu) (k-1)
  in aux [] i
*)

type expr = 
| EVar of Aiger.Symbol.t
| ENot of expr
| EAnd of expr * expr
| EOr of expr * expr
| EXor of expr * expr
| ETrue 
| EFalse

let name_cnt = ref 0


let aux_expr_to_declarations name expr =
  let new_name () = 
    incr name_cnt;
    "wire"^string_of_int !name_cnt
  in
  let rec aux accu = function 
    | ETrue -> accu,True
    | EFalse -> accu,False
    | EVar s -> accu,Symb s
    | ENot e -> 
      let a,s = aux accu e in
      a , (match s with | True -> False | False -> True 
      | Symb s -> Not s | Not s -> Symb s)

    | EAnd (e,f) -> 
      let a,se = aux accu e in
      let a,sf = aux a f in
      let sym = new_name (),None in
      EqualAnd(sym,se,sf) (*:: Wire sym*) :: a, Symb sym
    | EOr (e,f) -> aux accu (ENot (EAnd (ENot e, ENot f)))
    | EXor (e,f) -> aux accu (EOr (EAnd (e, ENot f), EAnd (f, ENot e)))
  in 
  let dec, w = aux [] expr in dec, w

let expr_to_declarations name expr =
  let dec, w = aux_expr_to_declarations name expr in 
  List.rev (EqualAnd (name,w,True) :: dec)

let expr_to_register_declarations name expr =
  let dec, w = aux_expr_to_declarations name expr in 
  List.rev (UpdateReg (name,w) :: dec)
      




