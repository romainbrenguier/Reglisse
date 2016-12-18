(*
 * Copyright 2014 Romain Brenguier
 * Author: Romain Brenguier <romain.brenguier@ulb.ac.be>
 * 
 * This file is part of Ocaml-aiger.
 * 
 * Ocaml-aiger is a free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, version 3.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details. 
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

type lit = int
let aiger_false = 0
let aiger_true = 1
let neg lit = if lit mod 2 = 0 then lit + 1 else lit - 1
let inverted lit = if lit mod 2 = 0 then false else true
let strip lit = if lit mod 2 = 0 then lit else lit - 1

let debug s = Printf.eprintf "[aigerImperative] %s\n" s

module LitSet = 
struct
  type t = { mutable lit_max:int ; lit_tab:(lit,int) Hashtbl.t}

  let make () = {lit_max=0; lit_tab=Hashtbl.create 100}
  let fold f set start = 
    Hashtbl.fold (fun lit i accu ->
      f ~index:i ~lit accu
    ) set.lit_tab start

  let add set lit = 
    Hashtbl.add set.lit_tab lit set.lit_max;
    set.lit_max <- set.lit_max+1

  let mem set lit =
    Hashtbl.mem set.lit_tab lit

  let remove set lit = 
    let start = Hashtbl.find set.lit_tab lit in
    Hashtbl.remove set.lit_tab lit;
    Hashtbl.iter (fun lit index ->
      if index > start 
      then Hashtbl.replace set.lit_tab lit (index - 1)
    ) set.lit_tab;
    set.lit_max <- set.lit_max - 1

  let elements set = fold (fun ~index ~lit accu -> lit :: accu) set []

  let iter f set = 
    fold (fun ~index ~lit () -> f lit) set () 

end
  
type t = {
  mutable maxvar:int;   
  mutable num_inputs:int;
  mutable num_latches:int;
  mutable num_outputs:int;
  mutable num_ands:int;

  inputs: LitSet.t;
  latches:(lit,lit) Hashtbl.t;
  outputs:LitSet.t;
  ands: (lit,lit*lit) Hashtbl.t;
  ands_inv: (lit*lit,lit) Hashtbl.t;
  symbols: (lit,string) Hashtbl.t;
  symbols_inv: (string,lit) Hashtbl.t;

  mutable comments:string list;
}

exception Correspondance_not_found of string


let gates aig = 
  let unsorted = 
    Hashtbl.fold
      (fun lhs (rhs0,rhs1) accu ->
	(lhs,rhs0,rhs1) :: accu
      ) aig.ands []
  in List.sort (fun (a,_,_) (b,_,_) -> compare a b) unsorted
      
let string2lit_exn aiger string = 
  try Hashtbl.find aiger.symbols_inv string
  with Not_found -> raise (Correspondance_not_found string)
let lit2string_exn aiger lit = 
  try Hashtbl.find aiger.symbols lit 
  with Not_found -> raise (Correspondance_not_found ("lit("^string_of_int lit^")"))
let string2lit aiger string = 
  try Some (Hashtbl.find aiger.symbols_inv string)
  with Not_found -> None
let lit2string aiger lit = 
  try Some ( Hashtbl.find aiger.symbols lit) 
  with Not_found -> None

let add_correspondance aiger lit symbol =
  Hashtbl.add aiger.symbols_inv symbol lit;
  Hashtbl.add aiger.symbols lit symbol


let change_correspondance aiger old_symbol new_symbol =
  try
    let lit = Hashtbl.find aiger.symbols_inv old_symbol in
    Hashtbl.replace aiger.symbols lit new_symbol;
    Hashtbl.remove aiger.symbols_inv old_symbol;
    Hashtbl.add aiger.symbols_inv new_symbol lit 
  with 
    Not_found -> Printf.eprintf "Warning: symbol %s was not previously defined\n" old_symbol


    
let empty () =   
  {maxvar=0; num_inputs=0; num_latches=0;
   num_outputs=0;num_ands=0;
   inputs=LitSet.make (); 
   latches=Hashtbl.create 100; 
   outputs=LitSet.make (); 
   ands=Hashtbl.create 100; 
   ands_inv=Hashtbl.create 100; 
   comments= []; 
   symbols = Hashtbl.create 100;   
   symbols_inv = Hashtbl.create 100;
  }

let parse_exn inch = 
  let line = input_line inch in
  let aiger = empty () in
  Scanf.sscanf line "aag %d %d %d %d %d" 
    (fun v a b c d -> 
      aiger.maxvar <- v; 
      aiger.num_inputs <- a;
      aiger.num_latches <- b;
      aiger.num_outputs <- c;
      aiger.num_ands <- d
    );

  let inputs = Array.make aiger.num_inputs aiger_false in
  let latches = Array.make aiger.num_latches aiger_false in
  let outputs = Array.make aiger.num_outputs aiger_false in

  let add_input index lit =
    Printf.printf "adding input %d\n" lit;
    LitSet.add aiger.inputs lit;
    inputs.(index) <- lit
  in
    
  for i = 0 to aiger.num_inputs - 1
  do
    try 
      let line = input_line inch in
      Scanf.sscanf line "%d" (add_input i)
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading input#%d\n" (i+1); 
      raise End_of_file
  done;

  let add_latch index lit up = 
    Hashtbl.add aiger.latches lit up;
    latches.(index) <- lit
  in

  for i = 0 to aiger.num_latches - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d %d" (add_latch i)
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading latch#%d\n" (i+1); 
      raise End_of_file
  done;

  let add_output index lit = 
    LitSet.add aiger.outputs lit;
    outputs.(index) <- lit
  in

  for i = 0 to aiger.num_outputs - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d" (add_output i)
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading output#%d\n" (i+1); 
      raise End_of_file
  done;

  let add_and lhs rhs0 rhs1 = 
    let rhs = min rhs0 rhs1, max rhs0 rhs1 in
    Hashtbl.add aiger.ands lhs rhs;
    Hashtbl.add aiger.ands_inv rhs lhs
  in

  for i = 0 to aiger.num_ands - 1 do 
    try 
      let line = input_line inch in 
      Scanf.sscanf line "%d %d %d" add_and
    with End_of_file -> 
      Printf.eprintf "Error: end of file reached while reading and#%d/%d\n" (i+1) aiger.num_ands; 
      raise End_of_file
  done;

  let stop = ref false in

  while (not !stop) do
    try 
      let line = input_line inch in
      let id,name = match Str.full_split (Str.regexp "[ ]") line with
	| Str.Text id :: Str.Delim " " :: Str.Text name :: _ -> id,name
	| _ -> failwith ("In Aiger.read_symbols: could not parse symbol "^line)
      in
      let lit = Scanf.sscanf id "%c%d"
	(fun t a -> match t with
	| 'i' -> inputs.(a)
	| 'o' -> outputs.(a)
	| 'l' -> latches.(a)
	| c -> failwith (Printf.sprintf "In Aiger.read_symbols: unknown variable type %c" c))
	  
      in add_correspondance aiger lit name
    with _ -> stop := true
  done;

  stop := false;
  
  while (not !stop) do
    try
      let line = input_line inch in
      aiger.comments <- line :: aiger.comments
    with End_of_file -> stop := true
  done; 
  aiger.comments <- List.rev aiger.comments;
  
  aiger

let read_exn inch = parse_exn inch

let read_from_file_exn file =  
  let inch = open_in file in
  let aiger = read_exn inch in
  close_in inch;
  aiger

let new_var t = t.maxvar <- t.maxvar+1; 2 * t.maxvar

exception AlreadyExists
    
let add_input t name =
  t.num_inputs <- t.num_inputs + 1;
  let lit = new_var t in
  LitSet.add t.inputs lit;
  add_correspondance t lit name;
  lit

let add_latch t name = 
  t.num_latches <- t.num_latches + 1;
  let lit = new_var t in
  Hashtbl.add t.latches lit aiger_false;
  add_correspondance t lit name;
  lit

let set_latch_update t lit upd =
  Hashtbl.replace t.latches lit upd
  
let set_output t name lit = 
  t.num_outputs <- t.num_outputs + 1;
  LitSet.add t.outputs lit;
  add_correspondance t lit name

let conj t rhs0 rhs1 = 
  if rhs0 = aiger_false || rhs1 = aiger_false then aiger_false
  else if rhs0 = aiger_true then rhs1 
  else if rhs1 = aiger_true then rhs0
  else 
    let rhs = min rhs0 rhs1, max rhs1 rhs0 in
    try Hashtbl.find t.ands_inv rhs
    with Not_found ->
      t.num_ands <- t.num_ands + 1;
      let lhs = new_var t in
      Hashtbl.add t.ands lhs rhs;
      Hashtbl.add t.ands_inv rhs lhs;
      lhs

let disj t rhs0 rhs1 = 
  neg (conj t (neg rhs0) (neg rhs1))

let add_comment t comment = t.comments <- comment :: t.comments


type tag = Constant of bool | Input of lit | Latch of (lit*lit) | And of (lit*lit*lit) | Output of lit

let lit2tag_exn t lit = 
  if lit < 2 then Constant (lit = 1)
  else 
    if LitSet.mem t.inputs lit
    then Input lit
    else
      if LitSet.mem t.outputs lit
      then Output lit
      else
	try Latch (lit, Hashtbl.find t.latches lit)
	with Not_found ->
	  let rhs0,rhs1 = Hashtbl.find t.ands lit in
	  And (lit,rhs0,rhs1)

let lit2tag t lit = 
  try Some (lit2tag_exn t lit) with Not_found -> None

exception Not_output of tag

let hide_exn t name = 
  let l = string2lit_exn t name in
  try
    if not (LitSet.mem t.inputs l || Hashtbl.mem t.latches l)
    then (Hashtbl.remove t.symbols_inv name;
	  Hashtbl.remove t.symbols l);
    LitSet.remove t.outputs l;
    t.num_outputs <- t.num_outputs - 1;
  with Not_found -> raise (Not_output (lit2tag_exn t l))


let names aiger =
  Hashtbl.fold (fun lit string accu ->
    string :: accu
  ) aiger.symbols []

let inputs_exn aiger = 
  LitSet.fold 
    (fun ~index ~lit accu -> 
      let name = lit2string_exn aiger lit in
      name :: accu
    ) aiger.inputs []

let latches_exn aiger = 
  Hashtbl.fold 
    (fun lit rhs accu -> 
      let name = lit2string_exn aiger lit in
      name :: accu
    ) aiger.latches []

let outputs_exn aiger = 
  LitSet.fold 
    (fun ~index ~lit accu ->
      Printf.printf "looking for output %d, %d\n" lit index;
      let name = lit2string_exn aiger lit in
      name :: accu
    ) aiger.outputs []
        
let write outch aiger =
  let inputs = 
    LitSet.fold (fun ~index ~lit accu -> (index,lit):: accu) aiger.inputs [] 
  |> List.sort (fun (a,_) (b,_) -> compare a b) 
  in
  let outputs = 
    LitSet.fold (fun ~index ~lit accu -> (index,lit):: accu) aiger.outputs [] 
  |> List.sort (fun (a,_) (b,_) -> compare a b) 
  in
  let latches = 
    Hashtbl.fold (fun lhs rhs accu -> lhs :: accu) aiger.latches [] 
  |> List.sort compare 
  |> List.fold_left (fun (i,accu) lhs -> i+1 , (i,lhs) :: accu) (0,[])
  |> snd
  |> List.rev
  in
  let ands = 
    Hashtbl.fold (fun lhs (rhs0,rhs1) accu -> (lhs,rhs0,rhs1) :: accu) aiger.ands [] 
  |> List.sort (fun (a,_,_) (b,_,_) -> compare a b)
  in


  Printf.fprintf outch "aag %d %d %d %d %d\n" aiger.maxvar aiger.num_inputs
    aiger.num_latches aiger.num_outputs aiger.num_ands;
  
  List.iter (fun (_,lit) -> Printf.fprintf outch "%d\n" lit) inputs ;
  List.iter
    (fun (_,lit) ->
      Printf.fprintf outch "%d %d\n" lit (Hashtbl.find aiger.latches lit)
    ) latches;
  
  List.iter (fun (_,lit) -> Printf.fprintf outch "%d\n" lit) outputs;
  List.iter (fun (a,b,c) -> Printf.fprintf outch "%d %d %d\n" a b c) ands;
  List.iter
    (fun (i,a) ->
      Printf.fprintf outch "i%d %s\n" i (lit2string_exn aiger a)
    ) inputs;
  
  List.iter
    (fun (i,lhs) ->
      Printf.fprintf outch "l%d %s\n" i (lit2string_exn aiger lhs)
    ) latches;
  
  (* Some outputs could have been removed *)
  List.iter
    (fun (i,a) -> Printf.fprintf outch "o%d %s\n" i (lit2string_exn aiger a)
    ) outputs;
  
  if aiger.comments <> [] then Printf.fprintf outch "c\n";
  List.iter (fun a -> Printf.fprintf outch "%s\n" a) aiger.comments


let write_to_file aiger file = 
  let outch = open_out file in
  write outch aiger;
  close_out outch

let copy aig =
  let latches = Hashtbl.copy aig.latches in
  let ands = Hashtbl.copy aig.ands in
  let ands_inv = Hashtbl.copy aig.ands_inv in
  let symbols = Hashtbl.copy aig.symbols in
  let symbols_inv = Hashtbl.copy aig.symbols_inv in
  {aig with latches; ands; ands_inv; symbols; symbols_inv} 
  
let compose aig1 aig2 =
  let map = Hashtbl.create aig2.num_ands in
  (* mapping from the literals of the second aig to the newly generated one *)
  let find x = 
    if inverted x then neg (Hashtbl.find map (strip x)) else Hashtbl.find map x
  in

  Hashtbl.add map aiger_true aiger_true;
  Hashtbl.add map aiger_false aiger_false;

  Hashtbl.iter
    (fun lit name -> 
      if Hashtbl.mem aig2.symbols_inv name;
      then Hashtbl.add map (string2lit_exn aig2 name) lit
    ) aig1.symbols;

  let aig = copy aig1 in
  
  (* add entry in the table for latches of the second module *)
  Hashtbl.iter
    (fun l n ->
      let a = lit2string_exn aig2 l in
      let lit = add_latch aig a in
      Hashtbl.add map l lit
    ) aig2.latches;

  LitSet.iter
    (fun i -> 
      (* if the input is not an output of the first module... *)
      if not (Hashtbl.mem map i)
      then (* ... we need to add it *)
	let lit = add_input aig (lit2string_exn aig2 i) in
	Hashtbl.add map i lit
    ) aig2.inputs;

  
  (* add entry in the table for gates of the second module *)
  (*Hashtbl.iter
    (fun g (l,r) -> 
      let lit = conj let na,nvar = new_var a in 
      Hashtbl.add map g (var2lit nvar); (* this is done when we add gates of the second module *)
      na
    ) aig2.ands;*)

  
  (* add inputs of the second module *)
  print_endline "AigerImperative: this has to be checked";
  (*
  LitSet.iter
    (fun i ->
      let name = lit2string_exn aig2 i in
      if not (Hashtbl.mem aig1.symbols_inv name)
      then
	(print_endline "this has to be checked";
	(*add_input aig (find i) name*)
	 add_input aig name
	)
    ) aig2.inputs;
  *)

  let rec recursive_add g =
    try find g
    with Not_found ->
      if inverted g
      then
	let l,r = Hashtbl.find aig2.ands (strip g) in
	let lit = conj aig (recursive_add l) (recursive_add r) in
	Hashtbl.add map g (neg lit);
	neg lit
      else 
	let l,r = Hashtbl.find aig2.ands g in
	let lit = conj aig (recursive_add l) (recursive_add r) in
	Hashtbl.add map g lit;
	lit
  in
  
  (* add gates of the second module *)
  Hashtbl.iter
    (fun g _ -> ignore (recursive_add g)
      (*(l,r) -> 
      let lit = conj aig (find l) (find r) in
	Hashtbl.add map g lit*)
    ) aig2.ands;

  (* add latches of the second module *)
  Hashtbl.iter 
    (fun l n -> 
      set_latch_update aig (find l) (find n)
    ) aig2.latches;

  (* add outputs of the second module *)
  LitSet.iter
    (fun o -> 
      set_output aig (lit2string_exn aig2 o) (find o) 
    ) aig2.outputs;

  print_endline "composition finished";
  aig


    
exception Circular_circuit of lit
exception Lit_not_found of lit
let order_gates_exn aig =
  let new_aig = empty () in

  debug "(* mapping from the literals of aig to the newly generated one *)";
  let map = Hashtbl.create aig.num_ands in
  Hashtbl.add map aiger_false aiger_false;
  let find x =
    try
      if inverted x then neg (Hashtbl.find map (strip x)) else Hashtbl.find map x
    with Not_found ->
      Printf.eprintf "[aigerImperative] lit not found: %d\n" x;
      raise Not_found	
  in
  
  debug "(* add entry in the table for latches *)";
  Hashtbl.iter
    (fun l n ->
      let a = lit2string_exn aig l in
      let lit = add_latch new_aig a in
      Hashtbl.add map l lit
    ) aig.latches;

  LitSet.iter
    (fun i -> 
      let lit = add_input new_aig (lit2string_exn aig i) in
      Hashtbl.add map i lit
    ) aig.inputs;

  let rec recursive_add visited g =
    if List.mem g visited then raise (Circular_circuit g);
    try find g
    with Not_found ->
      if inverted g
      then
	let l,r =
	  try Hashtbl.find aig.ands (strip g)
	  with Not_found -> raise (Lit_not_found g)
	in
	let lit = conj new_aig
	  (recursive_add (g::visited) l)
	  (recursive_add (g::visited) r)
	in
	Hashtbl.add map g (neg lit);
	neg lit
      else 
	let l,r =
	  try Hashtbl.find aig.ands g
	  with Not_found -> raise (Lit_not_found g)
	in
	let lit =
	  conj new_aig (recursive_add (g::visited) l)
	    (recursive_add (g::visited) r)
	in
	Hashtbl.add map g lit;
	lit
  in
  
  debug "(* add gates *)";
  Hashtbl.iter (fun g _ -> ignore (recursive_add [] g)) aig.ands;

  debug "(* add latches *)";
  Hashtbl.iter 
    (fun l n -> set_latch_update new_aig (find l) (find n)
    ) aig.latches;

  debug "(* add outputs *)";
  LitSet.iter
    (fun o -> set_output new_aig (lit2string_exn aig o) (find o) 
    ) aig.outputs;

  new_aig

    
let rename aig renaming =
  Hashtbl.iter
    (fun lit name ->
      Hashtbl.remove aig.symbols_inv name
    ) aig.symbols;
  Hashtbl.iter
    (fun lit name ->
      Hashtbl.replace aig.symbols lit (renaming name);
      Hashtbl.add aig.symbols_inv (renaming name) lit
    ) aig.symbols

let rename_list aiger renaming =
  names aiger 
  |>  List.iter (fun name -> if renaming name <> name then change_correspondance aiger name (renaming name))


  

