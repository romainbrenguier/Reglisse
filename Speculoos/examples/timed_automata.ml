(* ocamlbuild -tag use_ocaml-cudd -tag use_ocaml-aiger examples/timed_automata.byte *)
module Expr = Expression

type clock = Expr.t

let clock name size = Expr.var name (Type.int size)
let clock_to_expr x = x

type operator = Less | LessEq | Eq | GreaterEq | Greater
let operator_to_expr = function
  | Less -> Expr.less
  | LessEq -> Expr.less_eq
  | Eq -> Expr.equals
  | GreaterEq -> Expr.greater_eq
  | Greater -> Expr.greater

type guard = Expr.t * operator * Expr.t

let guard_to_expr (c,o,i) dt =
  (operator_to_expr o) (clock_to_expr (Expr.add c dt)) i

let guards_to_expr gl dt =
  List.fold_left
    (fun accu g -> 
      Expr.conj accu (guard_to_expr g dt)
    ) (Expr.bool true) gl

type location = {id : int; invariant : Expr.t }
type transition = {src : int; dst : int; guards : guard list; resets : clock list }

  

type t = {clocks: clock list;
	  locations: location list; 
	  transitions: transition list;
	  maxval : int}


let to_spec auto =
  let llog = Common.log (List.length auto.locations) in
  let mlog = Common.log auto.maxval in
  let location = Expr.var "location" (Type.int llog) in
  let dest = Expr.var "destination" (Type.int llog) in
  let dt = Expr.var "time_elapse" (Type.int mlog) in
  let max_location = List.fold_left (fun m l -> max m l.id) 0 auto.locations in
  let invariant_array = Array.make (max_location + 1) (Expr.bool true) in
  List.iter (fun l -> invariant_array.(l.id) <- l.invariant) auto.locations;

  let constraint_expression trans =
    Expr.conj
      (Expr.equals dest (Expr.int trans.dst))
      (Expr.conj 
	 (Expr.equals location (Expr.int trans.src))
	 (guards_to_expr trans.guards dt))
  in

  let clock_update trans clock = 
    if List.mem clock trans.resets then Expr.int 0 else Expr.add clock dt
  in

  let location_update trans = Expr.int trans.dst in


  let new_location =
    List.fold_left 
      (fun accu t ->
       Expr.ite (constraint_expression t)
		(location_update t)
		accu
      ) (Expr.int 0) auto.transitions
  in 

  let new_clock c =
    List.fold_left 
      (fun accu t ->
       Expr.ite (constraint_expression t)
		(clock_update t c)
		accu
      ) (Expr.int 0) auto.transitions
  in 

  List.fold_left 
    (fun accu c -> 
     (c, new_clock c) :: accu
    ) [location, new_location] auto.clocks 


let to_aiger auto = 
  let spec = to_spec auto in
  Cudd.init 100;
  Expr.functional_synthesis spec

let write_aiger t name =
  let outch = open_out name in
  Aiger.write (to_aiger t) outch;
  close_out outch

let test () = 
  let c = clock "clock1" 8 in
  let loc0 = {id=0; invariant = Expr.bool true} in
  let loc1 = {id=1; invariant = Expr.bool true} in
  let trans0 = {src=0; dst=0; guards=[c,LessEq,Expr.int 35]; resets=[]} in
  let trans1 = {src=0; dst=1; guards=[c,Greater,Expr.int 35]; resets=[]} in
  let trans2 = {src=1; dst=1; guards=[c,LessEq,Expr.int 23]; resets=[]} in
  let trans3 = {src=1; dst=1; guards=[c,Greater,Expr.int 23]; resets=[c]} in
  let auto = {clocks=[c];locations=[loc0;loc1];
	      transitions=[trans0;trans1;trans2;trans3]; maxval=511}
  in
  print_endline "writing aiger file to timed_auto.aag";
  write_aiger auto "timed_auto.aag"


let main = 
  if Filename.check_suffix Sys.argv.(0) "timed_automata" 
     || Filename.check_suffix Sys.argv.(0) "timed_automata.byte" 
  then test ()
  else ()


(*  
let dc = 1
let vuc = 1
let cp = 50
let dd = 
  [|
    [|0; 186; 206; 161; 181; 151; 190; 160|];
    [|186; 0; 20; 36; 30; 42; 50; 53|];
    [|206; 20; 0; 50; 36; 58; 50; 64|];
    [|161; 36; 50; 0; 20; 10; 36; 20|];
    [|181; 30; 36; 20; 0; 30; 22; 28|];
    [|151; 42; 58; 10; 30; 0; 44; 22|];
    [|190; 50; 50; 36; 22; 44; 0; 30|];
    [|160; 53; 64; 20; 28; 22; 30; 0|]|]
let vehicleAt = Type.make "vehicleAt" (Type.Array(Type.Int(8),2))


let invariantIdle id = 
  gc <= (deadline - dd[vehicleAt[id]][0]) & (cost' = vuc)

let invariantDriving id = 
  c <= busy[id] && cost' = dc + vuc

let invariantUnloading id =
  c <= busy[id] && cost' = vuc

let invariantDrivingHome id =
  c <= busy[id] && cost' = dc + vuc

let invariantHome id = 
  cost' = 0

let transition0 = 
  location = id0 && 
  next location = id1 &&
  drive[id] &&
  next c = 0

input unload[id]

let transition1 =
  location = id1 &&
  next location = id2 &&
  unload[id] &&
  c = 0
  
let main =
  transition0 || transition1
*)
