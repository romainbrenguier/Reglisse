open Common

type t = 
| EUnit
| EBool of Boolean.t 
| EInt of Integer.t
| EArray of t array
| ERecord of (string * t) list
| EConstr of (string * t)

let to_string = 
  let rec aux = function
    | EUnit -> "()"
    | EBool x -> Boolean.to_string x
    | EInt x -> Integer.to_string x
    | EArray arr -> 
      Array.fold_left (fun accu e -> accu^aux e^";") "[| " arr ^ "|]"
    | ERecord sel -> 
      List.fold_left (fun accu (s,e) -> accu^s^"= ("^aux e^") ;") "{ " sel ^ "}"
  in aux 

  

let var name typ = 
  let rec aux prefix = function
    | Type.Unit -> EUnit
    | Type.Bool -> EBool (Integer.to_boolean (Integer.bool_var prefix))
    | Type.Int i -> EInt (Integer.var prefix i)

    | Type.Array (t,i) -> 
      let arr = 
	Array.init i
	  (fun i -> 
	    let pre = Printf.sprintf "%s[%d]" prefix i in
	    aux pre t) 
      in
      EArray arr

    | Type.Record st -> 
      let list =
	List.map
	  (fun (name,t) -> 
	    let pre = Printf.sprintf "%s.%s" prefix name in
	    (name, aux pre t)
	  ) st
      in
      ERecord list
	
    | Type.Union stl -> 
      let cst_size, cnt_size = Type.union_sizes (Type.Union stl) in
      aux prefix (Type.Record ["constr",Type.Int cst_size; "content",Type.Int cnt_size])
	
  in 
  aux name typ 



(*
let make decl name typ = 
  let rec aux prefix = function
    | Type.Unit -> [],EUnit
    | Type.Bool -> let d,v = decl prefix None in [d],EBool v
    | Type.Int i -> let d,v = decl prefix (Some i) in [d],EInt v

    | Type.Array (t,i) -> 
      let dv_array = 
	Array.init i
	  (fun i -> 
	    let pre = Printf.sprintf "%s[%d]" prefix i in
	    aux pre t) 
      in
      Array.fold_left (fun accu (d,i)-> List.rev_append d accu) [] dv_array,
      EArray (Array.map snd dv_array)

    | Type.Record st -> 
      let dv_list =
	List.map
	  (fun (name,t) -> 
	    let pre = Printf.sprintf "%s.%s" prefix name in
	    let d,v = aux pre t in
	    d , (name, v)
	  ) st
      in
      List.fold_left (fun a (d,_) -> List.rev_append d a) [] dv_list,
      ERecord (List.map snd dv_list)
	
    | Type.Union stl -> 
       let max_size = List.fold_left (fun m (_,t) -> max (Type.size t) m) 0 stl in
       aux prefix
	   (Type.Record
	      ["constr",Type.Int (log (List.length stl));
	       "content",Type.Int max_size])
	 
  in 
  let dec,var = aux name typ in
  Synthesis.DList dec, var

*)


(*

let declare decl name t = 
  let rec aux prefix = function
    | EUnit -> 
    | EBool x | EInt x -> 
      let v,dx = decl x in
      v :: accu, dx :: d
    | EArray a -> 
      let _, accu = 
	Array.fold_left
	  (fun (i,accu) t -> 
	    i+1,aux (Printf.sprintf "%s[%d]" prefix i) accu t
	  ) (0,accu) a
      in accu
    | ERecord dl -> 
      List.fold_left
	(fun accu (name,t) -> 
	  aux (Printf.sprintf "%s.%s" prefix name) accu t
	) accu dl
  in
  let a = make name t in
  Speculog.DList (aux name [] a), a
*)


let to_integer = function
  | EBool x -> Integer.of_boolean x 
  | EInt x -> x
  | EUnit -> failwith "In Expression.to_expr: this is a unit expression"
  | EArray _ -> failwith "In Expression.to_expr: this is an array and not a atomic expression"
  | ERecord _ -> failwith "In Expression.to_expr: this is an tuple and not a atomic expression"

let rec to_type = function
  | EBool _ -> Type.bool
  | EUnit -> Type.unit
  | EInt i -> Type.int (Integer.size i)
  | EArray a -> 
    let length = Array.length a in
    if length > 0 then Type.array (to_type a.(0)) length
    else failwith "in Expression.to_type: cannot deduce the type of an empty array"
  | ERecord d -> Type.record (List.map (fun (s,x) -> s, to_type x) d)

let size = function
  | EArray a -> Array.length a
  | _ -> failwith "in Expression.size: the argument should be an expression of type array"

let get a index = 
  let i = match index with EInt i -> i | _ -> failwith "In Expression.get: the index is not an integer"
  in
  let rec aux array = match array.(0) with
    | EUnit -> EUnit
    | EBool _ -> EBool (Integer.to_boolean (Integer.multiplex i (Array.map to_integer array)))
    | EInt _ -> EInt (Integer.multiplex i (Array.map to_integer array))
    | EArray a -> 
      EArray (Array.init (Array.length a) 
			 (fun j -> aux (Array.map (function EArray x -> x.(j) | _ -> failwith "This value should only  contain arrays") array)))
    | ERecord d -> 
       ERecord (List.map 
		  (fun (name,field) -> 
		   name, aux (Array.map (function ERecord x -> List.assoc name x | _ -> failwith "This array should only contain records") array)) d)

  in match a with
  | EArray x -> aux x
  | _ -> failwith "In Expression.get: this is not an array"
    
let field a name = match a with
  | ERecord x -> 
    (try List.assoc name x
     with Not_found -> failwith ("In Expression.field: no field named "^name))
  | _ -> failwith "In Expression.field: this is not a tuple"

(* Warning this should only be used for commutative operators *)
let apply f a b = 
  let rec aux a b = match a, b with
    | EBool x, EBool y -> EBool (Integer.to_boolean (f (Integer.of_boolean x) (Integer.of_boolean y)))
    | EInt x, EInt y -> EInt (f x y)
    | EBool x, EInt y -> EInt (f (Integer.of_boolean x) y)
    | EInt y, EBool x -> EInt (f y (Integer.of_boolean x))

    | EArray x, EArray y ->
      if Array.length x <> Array.length y
      then failwith "In Expression.apply: the two arrays have different sizes"
      else EArray (Array.init (Array.length x) (fun i -> aux x.(i) y.(i)))
    | ERecord x, ERecord y ->
      ERecord 
	(List.fold_left 
	   (fun accu (name,field) -> 
	     let fy = try Some (List.assoc name y) with Not_found -> None in
	     match fy with 
	     | Some field2 -> (name,aux field field2) :: accu
	     | None -> failwith ("In Expression.apply: the second argument has no field named "^name)
	   ) [] x)
    | EUnit , x
    | x, EUnit -> failwith "In Expression.apply: operator applied to unit expression"
    | _ -> 
       print_endline ("the two elements have different types: "^to_string a^" and "^to_string b);
       failwith ("In Expression.apply: the two elements have different types: "^to_string a^" and "^to_string b)
  in aux a b
  

let apply1 op a = 
  let rec aux = function 
    | EUnit -> EUnit
    | EBool x -> EBool (Integer.to_boolean (op (Integer.of_boolean x)))
    | EInt x -> EInt (op x)
    | EArray x -> EArray (Array.map aux x)
    | ERecord x -> ERecord (List.map (fun (n,f) -> (n,aux f)) x)
  in aux a

let select a list = apply1 (fun x -> Integer.select x list) a
(* match a with 
  | EInt x -> EInt (Integer.select x list)
  | _ -> failwith "In Expression.select: this is not an integer"*)

let neg = apply1 Integer.neg

let apply1_bool op a =   
  let rec aux = function 
    | EUnit -> EUnit
    | EBool x -> EBool (op (Integer.of_boolean x))
    | EInt x -> EBool (op x)
    | EArray x -> EArray (Array.map aux x)
    | ERecord x -> ERecord (List.map (fun (n,f) -> (n,aux f)) x)
  in aux a

let andR,orR,xorR = match List.map apply1_bool [Integer.andR;Integer.orR;Integer.xorR] with [a;b;c] -> a,b,c | _ -> failwith "wrong number of results"
  
let implies,equiv,conj,disj,xor,add,minus,mult,div,modulo =
  match List.map apply [Integer.implies;Integer.equiv;Integer.conj;Integer.disj;Integer.xor;Integer.add;Integer.minus;Integer.mult;Integer.div;Integer.modulo] with 
  | [a;b;c;d;e;f;g;h;i;j] -> a,b,c,d,e,f,g,h,i,j
  | _ -> failwith "wrong number of results"

let unit = EUnit
let int i = EInt (Integer.int i)
let bool b = EBool (if b then Boolean.True else Boolean.False)

let left_shift e i = match e with
  | EInt a -> EInt (Integer.left_shift a i)
  | _ -> failwith "In Expression.left_shift: expression is not an integer"

let right_shift e i = match e with
  | EInt a -> EInt (Integer.right_shift a i)
  | _ -> failwith "In Expression.right_shift: expression is not an integer"

let equals a b =
  let rec aux = function
    | EUnit -> failwith "In Expression.equals: element is unit"
    | EBool x -> x
    | EInt x -> Integer.to_boolean x
    | EArray a -> 
      Array.fold_left
	(fun accu e -> Boolean.conj (aux e) accu)
	Boolean.True a

    | ERecord l -> 
      List.fold_left
	(fun accu (s,e) -> Boolean.conj (aux e) accu)
	Boolean.True l

  in 
  apply (fun x y -> Integer.equals x y |> Integer.of_boolean) a b 
  |> aux |> (fun x -> EBool x)

let comparison op a b =
  let rec aux op = function
    | EInt a, EInt b -> EBool (op a b)
    | _ -> failwith "In Expression.comparison: compared values are not integers"
  in aux op (a,b)

let less_eq,less,greater_eq,greater = 
  match List.map comparison [Integer.less_eq;Integer.less;Integer.greater_eq;Integer.greater] with
  | [a;b;c;d] -> a,b,c,d
  | _ -> failwith "Wrong number of results"


let ($@) = get
let ($.) = field

let ite i t e =
  match i with 
  | EBool x -> apply (Integer.ite x) t e
  | _ -> failwith "In Expression.ite: the condition is not a Boolean"

let mux c x = 
  EInt (Integer.multiplex (to_integer c) (Array.map to_integer x))


let of_int typ a = 
  let rec aux rem = function
    | Type.Unit -> EUnit, rem
    | Type.Bool -> 
      EBool rem.(0), Array.sub rem 1 (Array.length rem - 1)
    | Type.Int i -> 
      EInt (Integer.make (Array.sub rem 0 i)), Array.sub rem i (Array.length rem - i)
    | Type.Array (t,n) -> 
      let arr = Array.make n EUnit in
      let rec loop rem i =
	if i = n then rem
	else 
	  let v, rem = aux rem t in
	  arr.(i) <- v;
	  loop rem (i+1)
      in
      let rem = loop rem 0 in
      EArray arr, rem
    | Type.Record stl ->
      let v,rem =
	List.fold_left 
	  (fun (accu,rem) (s,t) -> 
	    let v,r = aux rem t in
	    (s,v) :: accu,r
	  ) 
	  ([],rem)
	  (List.sort (fun (s,_) (t,_) -> String.compare s t) stl)
      in
      ERecord v, rem
	
    | Type.Union stl ->
      let size_cs,size_ct = Type.union_sizes (Type.Union stl) in
      let constr,rem = aux rem (Type.Int size_cs) in
      let content,rem = aux rem (Type.Int size_ct) in
      (ERecord ["constr",constr;"content",content]), rem
  in 
  match a with 
  | EInt i ->
    let res,rem = aux (Integer.to_boolean_array i) typ in
    (* if Array.length rem > 0 
    (*This can happen in union type *)
       then 
       (
       Printf.printf "Result : %s\n" (to_string res);
       Printf.printf "Remaing size : %d\n" (Array.length rem);
       failwith "In Expression.of_int: element has not the correct size"
       );*)
    res
  | _ -> failwith "In Expression.of_int: element is not an int"


let to_int typ a = 
  let rec aux sum typ elt = match typ,elt with
    | Type.Unit, EUnit -> sum
    | Type.Bool , EBool b -> 
      Array.init (Array.length sum + 1) 
	(fun i -> if i = 0 then b else sum.(i-1))
    | Type.Int i , EInt e -> 
      Array.init (Array.length sum + i) 
	(fun j -> if j < i then Integer.get e j else sum.(j-i))
    | Type.Array(t,n), EArray arr -> 
       (*let size = Array.length arr in*)
       let rec loop sum i =
	 if i = n then sum
	 else 
	   let sum = aux sum t arr.(i) in
	   loop sum (i+1)
       in
       loop sum 0 
    | Type.Record stl, ERecord sel ->
      List.fold_left 
	(fun sum (s,t) -> 
	  let elt = List.assoc s sel in
	  aux sum t elt
	) sum
	(List.sort (fun (s,_) (t,_) -> String.compare s t) stl)

    | Type.Union stl, ERecord ["constr",constr;"content",content] ->
      let size_cs,size_ct = Type.union_sizes (Type.Union stl) in
      let sum = aux sum (Type.Int size_cs) constr in
      aux sum (Type.Int size_ct) content

    | x , e -> failwith ("In Expression.to_int: the element is not of type "^Type.to_string x^", its value is "^to_string e)
	
  in EInt (Integer.make (aux [| |] typ a))


let constr typ name a = 
  match typ with
  | Type.Union stl ->
    let constr = 
      let rec loop i = function
	| (x,y) :: tl when x = name -> int i
	| hd :: tl -> loop (i+1) tl
	| _ -> failwith ("In Expression.constr: type "^Type.to_string typ^" has no constructor "^name)
      in loop 0 (List.sort (fun (s,_) (t,_) -> String.compare s t) stl)
    in
    let content = to_int (List.assoc name stl) a in
    ERecord ["constr",constr;"content",content]
  | t -> failwith ("In Expression.constr: type "^Type.to_string t ^" is not a union type")

let array arr = EArray arr
  
let record list = ERecord list

let match_case typ a constr =
  match typ with 
  | Type.Union stl ->
     (*let length = List.length stl in*)
    let typ = List.assoc constr stl in
    (typ,of_int typ (field a "content"))
  | _ -> failwith "In Expression.match_case: type is not a union"

let match_with typ a patterns =
  match typ with 
  | Type.Union stl ->
    let length = List.length stl in
    let tab = Hashtbl.create length in
    let typs = Hashtbl.create length in
    let _ = 
      List.fold_left
	(fun i (s,t) -> 
	  Hashtbl.add tab s i; Hashtbl.add typs s t; i+1
	) 0 stl 
    in
    let matched = Hashtbl.create length in
    let expr_of_pat pat expr = 
      let index = Hashtbl.find tab pat in
      Hashtbl.add matched index true;
      (expr (of_int (Hashtbl.find typs pat) (field a "content"))) 
    in
    let expr = match patterns with
      | (phd,ehd) :: tl ->
	 List.fold_left 
	   (fun accu (pat,expr) -> 
	    try
	      let index = Hashtbl.find tab pat in
	      let e = expr_of_pat pat expr in
	      ite (equals (field a "constr") (EInt (Integer.int index)))
		e accu
	    with Not_found -> failwith ("In Expression.match_with: type "^Type.to_string typ^" has no constructor "^pat)
	   ) (expr_of_pat phd ehd) tl
      | _ -> failwith "In Expression.match_with: empty pattern"
    in 
    Hashtbl.iter 
      (fun s i -> 
	let b = try Hashtbl.find matched i with Not_found -> false in
	if not b 
	then failwith ("In Expression.match_with: this pattern-matching is not exhaustive: "^s^" is not matched")
      ) tab;
    expr
	
  | _ -> failwith "In Expression.match_with: type is not a union"
  


let for_each list f =
  let treat_one accu (start,last) =
    iter start last (fun i c -> conj (f i) c) accu
  in 
  List.fold_left treat_one (bool true) list

let for_some list f =
  let treat_one accu (start,last) =
    iter start last (fun i c -> disj (f i) c) accu
  in 
  List.fold_left treat_one (bool false) list





let remove_input aiger inp = 
  let inputs = List.filter (fun l -> l <> inp) aiger.Aiger.inputs in
  let _,gates = List.fold_left 
    (fun (to_remove,accu) (a,b,c) -> 
      if List.mem (Aiger.strip b) to_remove || List.mem (Aiger.strip c) to_remove 
      then (a :: to_remove, accu)
      else (to_remove, (a,b,c) :: accu)
    ) ([inp],[]) aiger.Aiger.ands
  in
  {aiger with Aiger.num_inputs = List.length inputs;
    Aiger.inputs = inputs;
    Aiger.ands = List.rev gates;
    Aiger.num_ands = List.length gates
  }

(*  
let set_latch aiger lit output =
  (* let update = List.assoc lit aiger.Aiger.latches in*)
  Common.debug (Printf.sprintf "setting latch %d -> %d\n" (Aiger.lit2int lit) (Aiger.lit2int output));
  (*let aiger =
    match Aiger.lit2tag aiger update with
    | Input i -> remove_input aiger i
    | _ -> Common.debug ("warning: the latch is not updated by an input"); aiger
  in*)
  let latches = List.map (fun (l,u) -> 
    if l = lit then (l,output) else (l,u)
  ) aiger.Aiger.latches
  in
  let outputs = List.filter (fun o -> o <> output) aiger.Aiger.outputs in
  let aiger = {aiger with
    Aiger.num_latches = List.length latches; (*aiger.Aiger.num_latches - 1;*)
    Aiger.latches = latches; 
    Aiger.num_outputs = List.length outputs; (*aiger.Aiger.num_outputs - 1;*)
    Aiger.outputs = outputs}
  in aiger	


let make_latch name typ generate = 
  let v = var name typ in
  let tmp_name = Common.tmp_name () in
  let fake = var tmp_name typ in
  let tmp_name2 = Common.tmp_name () in
  let fake_input = var tmp_name2 typ in
  let tmp_name3 = Common.tmp_name () in
  let next_value = var tmp_name3 typ in
  let aig = functional_synthesis [Update(fake,v); Update(v, fake_input)] in
  let generated = generate fake next_value in
  let composed = Aiger.compose aig generated in
  let lits_of_symbols = 
    List.fold_left (fun accu (s,t) -> 
      let u = 
	try Aiger.symbol2lit composed t
	with Not_found -> 
	  Common.debug ("warning "^Aiger.Symbol.to_string s^","^Aiger.Symbol.to_string t^" not found\n"); 
	  Aiger.aiger_false
      in (Aiger.symbol2lit composed s, u) :: accu
    ) []
  in
  
  let lits = lits_of_symbols (List.combine (to_symbols composed v) (to_symbols composed next_value)) in
  let with_latch = List.fold_left (fun aig (lit,olit) -> 
    set_latch aig lit olit) composed lits 
  in
  let aiger = hiding with_latch tmp_name typ in

  let lits_of_symbols = 
    List.fold_left (fun accu s -> 
      try Aiger.symbol2lit aiger s :: accu
      with Not_found -> accu
    ) []
  in
  let lits = lits_of_symbols (to_symbols aiger fake_input) in
  let res = List.fold_left remove_input aiger lits in
  res

let test () = 
  let union_type = Type.Union ["Int",Type.Int 2;"Bool",Type.Bool] in
  let typ = Type.of_string "{i: Int of int 2 | Bool of bool; a : int 3} [4]" in
  print_endline (Type.to_string typ);
  let d,e = input "test" typ in
  (*let f = to_expr (field (field (get e (EInt (Integer.int 3))) "i") "content") in*)
  let f = field (get e (int 3)) "i" in
  let m = match_with union_type f ["Int",(fun i -> int 0);"Bool",neg] in
  print_endline (to_string m)
  
  
  
let main = 
  if Filename.basename Sys.argv.(0) = "expression.byte" 
  then
    if Array.length Sys.argv < 1
    then
      Printf.printf "usage : %s\n" Sys.argv.(0)
    else
      test ()
(*let stream = Stream.of_string Sys.argv.(1) in
      let t = parse (lexer stream) in
      print_endline (to_string t)*)

*)
