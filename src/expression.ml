open Common

type t = 
| EUnit
| EBool of unit Integer.t 
| EInt of unit Integer.t
| EArray of t array
| ERecord of (string * t) list
(*| EUnion of 'a Integer.t * 'a Integer.t*)

let to_string = 
  let rec aux = function
    | EUnit -> "()"
    | EBool x | EInt x -> Integer.to_string x
    | EArray arr -> 
      Array.fold_left (fun accu e -> accu^aux e^";") "[| " arr ^ "|]"
    | ERecord sel -> 
      List.fold_left (fun accu (s,e) -> accu^s^"= ("^aux e^") ;") "{ " sel ^ "}"
  in aux 

  

let var name typ = 
  let rec aux prefix = function
    | Type.Unit -> EUnit
    | Type.Bool -> EBool (Integer.var prefix 1)
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



let make decl name typ = 
  let rec aux prefix = function
    | Type.Unit -> [],EUnit
    | Type.Bool -> let d,v = decl prefix 1 in [d],EBool v
    | Type.Int i -> let d,v = decl prefix i in [d],EInt v

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
  Speculog.DList dec, var

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

let input = make Speculog.input
let output = make Speculog.output
let reg = make Speculog.reg
let wire = make Speculog.wire


let to_expr = function
  | EBool x | EInt x -> x
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
    | EBool _ -> EBool (Integer.multiplex i (Array.map to_expr array))
    | EInt _ -> EInt (Integer.multiplex i (Array.map to_expr array))
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

let apply f a b = 
  let rec aux a b = match a, b with
    | EUnit , x | x, EUnit -> aux (EBool (Integer.bool false)) x
    | EBool x, EBool y -> EBool (f x y)
    | EInt x, EInt y | EBool x, EInt y | EInt y, EBool x
      -> EInt (f x y)

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
    | _ -> 
       print_endline ("the two elements have different types: "^to_string a^" and "^to_string b);
       failwith ("In Expression.apply: the two elements have different types: "^to_string a^" and "^to_string b)
  in aux a b
  

let apply1 op a = 
  let rec aux = function 
    | EUnit -> EUnit
    | EBool x -> EBool (op x)
    | EInt x -> EInt (op x)
    | EArray x -> EArray (Array.map aux x)
    | ERecord x -> ERecord (List.map (fun (n,f) -> (n,aux f)) x)
  in aux a

let neg,next = match List.map apply1 [Integer.neg;Integer.next] with [a;b] -> a,b | _ -> failwith "wrong number of results"

  
let implies,equiv,conj,disj,xor,add,minus,mult,div,modulo =
  match List.map apply [Integer.implies;Integer.equiv;Integer.conj;Integer.disj;Integer.xor;Integer.add;Integer.minus;Integer.mult;Integer.div;Integer.modulo] with 
  | [a;b;c;d;e;f;g;h;i;j] -> a,b,c,d,e,f,g,h,i,j
  | _ -> failwith "wrong number of results"

let unit = EUnit
let int i = EInt (Integer.cast (Integer.int i))
let bool b = EBool (Integer.cast (Integer.bool b))

let left_shift e i = match e with
  | EInt a -> EInt (Integer.left_shift a i)
  | _ -> failwith "In Expression.left_shift: expression is not an integer"

let right_shift e i = match e with
  | EInt a -> EInt (Integer.right_shift a i)
  | _ -> failwith "In Expression.right_shift: expression is not an integer"

let equals a b =
  let rec aux = function
    | EUnit -> failwith "In Expression.equals: element is unit"
    | EBool x -> EBool x
    | EInt x -> EBool x
    | EArray a -> 
      Array.fold_left
	(fun accu e ->
	  conj (aux e) accu
	) (EBool (Integer.bool true)) a

    | ERecord l -> 
      List.fold_left
	(fun accu (s,e) ->
	  conj (aux e) accu
	) (EBool (Integer.bool true)) l

  in aux (apply Integer.equals a b )

let comparison op a b =
  let rec aux = function
    | EInt x -> EBool x
    | _ -> failwith "In Expression.comparison: element is not an integer"
  in aux (apply op a b )

let less_eq,less,greater_eq,greater = 
  match List.map comparison [Integer.less_eq;Integer.less;Integer.greater_eq;Integer.greater] with
  | [a;b;c;d] -> a,b,c,d
  | _ -> failwith "Wrong number of results"

let ite i t e =
  match i with 
  | EBool x -> apply (Integer.ite (Integer.cast x)) t e
  | _ -> failwith "In Expression.ite: the condition is not a Boolean"

let of_int typ a = 
  let rec aux rem = function
    | Type.Unit -> EUnit, rem
    | Type.Bool -> 
      EBool (Integer.cast (Integer.of_boolean rem.(0))), Array.sub rem 1 (Array.length rem - 1)
    | Type.Int i -> 
      EInt (Integer.make None (Array.sub rem 0 i)), Array.sub rem i (Array.length rem - i)
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
	(fun i -> if i = 0 then Integer.get b 0 else sum.(i-1))
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
	
  in EInt (Integer.make None (aux [| |] typ a))


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
	      ite (equals (field a "constr") (EInt (Integer.cast (Integer.int index))))
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
  

let init initial updates =
  (*let decl, initialized = reg "initialized" Bool in*)
  let initialized = var "initialized" Type.Bool in
  List.fold_left 
    (fun accu (var,up) ->
     let i = 
       try List.assoc var initial
       with Not_found -> bool false
     in (var, ite initialized up i) :: accu
    ) [initialized,bool true] updates


let functional_synthesis t2list =
  let rec aux accu = function
    | EUnit , EUnit -> accu
    | EBool x, EBool y | EInt x, EInt y | EBool x, EInt y | EInt x, EBool y -> (x,y) :: accu
    | EArray arr1, EArray arr2 ->
      fst (Array.fold_left
	     (fun (accu,i) t -> 
	       aux accu (t,arr2.(i)) ,i+1
	     ) (accu,0) arr1)
    | ERecord stl1, ERecord stl2 ->
      List.fold_left 
	(fun accu (s,t) -> 
	  aux accu (t, List.assoc s stl2)
	) accu stl1
    | _ -> failwith "In Expression.functional_synthesis: the two elements are not of the same type"
  in
  let list = List.fold_left aux [] t2list in
  Speculog.functional_synthesis list 


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


let to_symbols aiger t = 
  let rec aux = function
    | EUnit -> []
    | EBool i | EInt i -> 
      let array = Integer.to_boolean_array i in
      Array.fold_left
	(fun accu b -> match b with 
	| Boolean.EVar (s,i) -> (s,Some i) :: accu
	) [] array
    | EArray a -> 
      Array.fold_left (fun accu c -> List.rev_append (aux c) accu) [] a
  in aux t

let rename aiger s typ name =
  let u = var s typ in
  let v = var name typ in
  let s1,s2 = to_symbols aiger u, to_symbols aiger v in
  let renaming = List.combine s1 s2 in
  Aiger.rename aiger renaming

let hiding aiger s typ = 
  let u = var s typ in
  let s = to_symbols aiger u in
  List.fold_left Aiger.hide aiger s

let use_module aiger ~inputs ~outputs generate =
  let aiger = 
    List.fold_left (fun aig (s,t) -> 
      let name = Common.tmp_name () in
      let typ = to_type t in
      let new_var = var name typ in
      let input = functional_synthesis [new_var, t] in
      let renamed = rename aig s typ name in
      let aig = Aiger.compose input renamed in
      hiding aig name typ
    ) aiger inputs 
  in
  let aiger = Aiger.full_rename aiger outputs (*input_renaming*) in
  let var_list = 
    List.fold_left 
      (fun accu (r,s) -> 
	(var s (Type.int (Aiger.size_symbol aiger s))) :: accu
      ) [] outputs
  in
  let gen_aiger = (*functional_synthesis*) (generate var_list) in
  let composed = Aiger.compose aiger gen_aiger in
  (*let hiden = List.fold_left (fun aig (r,s) -> Aiger.full_hide aig s) composed outputs in*)
  composed


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
  
let set_latch aiger lit output =
  let update = List.assoc lit aiger.Aiger.latches in
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
  let aig = functional_synthesis [fake,v; v, fake_input] in
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

