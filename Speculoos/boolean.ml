open Common

type t = 
| EVar of string
(*| ESimple of string
| EVar of string * int
| ESimpleNext of string
| ENext of string * int*)
| EExists of t list * t
| EForall of t list * t
| ENot of t
| EAnd of t list
| EOr of t list
| EList of t list
| True 
| False

let rec to_string = function 
  | EVar s -> Printf.sprintf "%s" s
  (*| ESimple s -> Printf.sprintf "%s" s
  | EVar (s,i) -> Printf.sprintf "%s.(%d)" s i
  | ENext (s,i) -> Printf.sprintf "next %s.(%d)" s i
  | ESimpleNext s -> Printf.sprintf "next %s" s*)
  | EExists (list,e) -> Printf.sprintf "(exists [%s]. %s)" (to_string (EList list)) (to_string e)
  | EForall (list,e) -> Printf.sprintf "(forall [%s]. %s)" (to_string (EList list)) (to_string e)
  | ENot a -> Printf.sprintf "(not %s)" (to_string a)
  | EAnd (hd :: tl) -> 
    List.fold_left (fun s a -> 
      Printf.sprintf "%s & %s" s (to_string a)
    ) ("("^to_string hd) tl ^")"
  | EOr (hd :: tl) -> 
    List.fold_left (fun s a -> 
      Printf.sprintf "%s || %s"  s (to_string a)
    ) ("("^to_string hd) tl ^")"
  | EList l -> List.fold_left (fun a b -> a ^ (to_string b)^"; ") "" l
  | EAnd [] | True -> "True"
  | EOr [] | False -> "False"
    
    
let var s = EVar (s)
(*let simple s = ESimple s
let next_var s i = ENext (s,i)
let next_simple s = ESimpleNext s
  
exception AlreadyNext of t
let rec next e = match e with
    | ESimple s -> ESimpleNext s
    | EVar (s,i) -> ENext (s,i)
    | ENext(_,_) | ESimpleNext _ -> raise (AlreadyNext e)
    | EExists (el,e) -> EExists (List.map next el, next e)
    | EForall (el,e) -> EForall (List.map next el, next e)
    | ENot x -> ENot (next x)
    | EAnd a -> EAnd (List.map next a)
    | EOr a -> EOr (List.map next a)
    | EList (el) -> EList (List.map next el)
    | True -> True
    | False -> False
*)

let forall vl e = match e with 
    | True | False -> e
    | _ -> EForall (vl,e)
let exists vl e = match e with 
    | True | False -> e
    | _ -> EExists (vl,e)

let of_list el = EList el

let insert b a = 
  let rec aux accu = function 
    | [] -> List.rev_append accu [a]
    | hd :: tl when hd > a -> aux (hd :: accu) tl
    | hd :: tl when hd = a -> List.rev_append accu (hd :: tl)
    | hd :: tl (* hd < a *) -> List.rev_append accu (a :: hd :: tl)
  in aux [] b

let append a l = List.fold_left insert l a

(** split list1 into a list of what belong to list2 and what does not *)
let partition list1 list2 =
  List.partition (fun x -> List.mem x list2) list1

let rec neg a = match a with
  | True -> False | False -> True 
  | ENot a -> a 
  | EOr a -> EAnd (List.map neg a)
  | EAnd a -> EOr (List.map neg a)
  | x -> ENot x
  
let disj a b = match a , b with
    | True, x | x , True -> True
    | False, x | x, False -> x
    | EOr a, EOr b -> EOr (append a b)
    | EOr a, b | b, EOr a -> EOr (insert a b)
    | EAnd a, EAnd b ->
      let p = partition a b in
      (*Printf.printf "a : %d ; b : %d ; p1 : %d; p2 : %d\n" (List.length a) (List.length b) (List.length (fst p)) (List.length (snd p));*)
      if (List.length (fst p)) > 0
      then EAnd (EOr([EAnd (snd p); EAnd (snd (partition b a))]) :: fst p)
      else EOr [EAnd a; EAnd b]
    | a, b -> if a = b then a else if a = neg b then True else EOr [a;b]

let conj a b = match a , b with
    | True, x | x , True -> x
    | False, x | x, False -> False
    | EAnd a, EAnd b -> EAnd (append a b)
    | EAnd a, b | b, EAnd a -> EAnd (insert a b)
    | EOr a, EOr b ->
      let p = partition a b in
      if (List.length (fst p)) > 0
      then EOr (EAnd([EOr (snd p); EOr (snd (partition b a))]) :: fst p)
      else EAnd [EOr a; EOr b]
    | a, b -> if a = b then a else if a = ENot b || ENot a = b then False else EAnd [a;b]
	

let xor a b = disj (conj a (neg b)) (conj (neg a) b)
let equals a b = disj (conj a b) (conj (neg a) (neg b))


let implies a b = disj b (neg a)
  
let for_each list f =
  let treat_one (start,last) =
    of_list (iter start last (fun i c -> (f i) :: c) [])
  in of_list (List.map treat_one list)

let ( $& ) = conj
let ( $| ) = disj
let ( $= ) = equals
let ( $^ ) = xor
let ( $=> ) = implies
  
module BddMap = Map.Make(struct type t = Cudd.bdd let compare = Cudd.compare end)

let of_bdd bdd symbols = 
  let insert bdd expr map = 
    (*print_endline (to_string expr);*)
    let n_expr =
      try disj (BddMap.find bdd map) expr
      with Not_found -> expr
    in BddMap.add bdd n_expr map
  in
(*bdd expr = 
    let rec aux accu = function
      | [] -> List.rev_append accu [(bdd,expr)]
      | (a,f) :: r -> 
	let c = Cudd.compare a bdd in
	if c < 0 then aux ((a,f)::accu) r
	else if c > 0 then List.rev_append accu ((bdd,expr) :: (a,f) :: r)
	else List.rev_append accu ((bdd, disjunction expr f)::r)
    in aux [] *)
    
  let aux bdd_expr_list name = 
    let var = AigerBdd.Variable.find name in
    BddMap.fold 
      (fun bdd expr accu->
	let var_bdd = AigerBdd.Variable.to_bdd var in
	let t = Cudd.bddRestrict bdd var_bdd in
	(*match opt with 
	| Some i ->
	  let expr_t = conj expr (EVar (name)) in
          let e = Cudd.bddRestrict bdd (Cudd.bddNot var_bdd) in
	  let expr_e = conj expr (ENot (EVar (name,i))) in
	
	  if Cudd.compare e t = 0
	  then insert e expr accu 
	  else
	    let accu = insert t expr_t accu in
	    let accu = insert e expr_e accu in
	    accu
	| None ->*)
	  let expr_t = conj expr (EVar name) in
          let e = Cudd.bddRestrict bdd (Cudd.bddNot var_bdd) in
	  let expr_e = conj expr (ENot (EVar name)) in
	
	  if Cudd.compare e t = 0
	  then insert e expr accu 
	  else
	    let accu = insert t expr_t accu in
	    let accu = insert e expr_e accu in
	    accu
      )  bdd_expr_list BddMap.empty
  in 
    
  let res = List.fold_left aux (BddMap.add bdd True BddMap.empty) symbols (*[bdd,True]*) in
  try BddMap.find (Cudd.bddTrue()) res
  with _ -> failwith "in [of_bdd]: missing variables"
      


let rec add_to_aiger aiger = function
  | EVar s -> AigerImperative.string2lit_exn aiger s
  | ENot t -> 
    let v = add_to_aiger aiger t in 
    AigerImperative.neg v
  | EAnd [t] ->  add_to_aiger aiger t 
  | EAnd (hd :: tl) -> 
    let v = add_to_aiger aiger hd in
    let w = add_to_aiger aiger (EAnd tl) in
    AigerImperative.conj aiger v w
  | EOr list -> add_to_aiger aiger (ENot (EAnd (List.map (fun x -> ENot x) list)))
  | x -> failwith ("in Boolean.add_to_aiger: unexpected expression : "^to_string x)
	      
let rec to_bdd = function
  | EVar x -> BddVariable.to_bdd (BddVariable.find x)
    (*  | ESimple x -> AigerBdd.Variable.to_bdd (AigerBdd.Variable.find (x,None))
	| ENext (x,i) -> AigerBdd.Variable.to_bdd (AigerBdd.Variable.next (AigerBdd.Variable.find (x,Some i)))
	| ESimpleNext x -> AigerBdd.Variable.to_bdd (AigerBdd.Variable.next (AigerBdd.Variable.find (x,None)))
  *)
  | EForall (vl,e) -> 
    let variables = 
      List.fold_left
	(fun accu e -> 
	  match e with 
	  | EVar x -> BddVariable.find x :: accu
    (*| ESimple x -> AigerBdd.Variable.find (x,None) :: accu*)
	  | _ -> failwith "In to_bdd: universal quantification on expressions that are not variables"
	) [] vl 
    in
    let cube = BddVariable.make_cube variables in
    Cudd.bddUnivAbstract (to_bdd e) cube
      
  | EExists (vl,e) -> 
    let variables = 
      List.fold_left
	(fun accu e -> 
	  match e with 
	  | EVar x -> BddVariable.find x :: accu
	 (*| ESimple x -> AigerBdd.Variable.find (x,None) :: accu*)
	  | _ -> failwith "In to_bdd: existential quantification on expressions that are not variables"
	) [] vl 
    in
    let cube = BddVariable.make_cube variables in
    Cudd.bddExistAbstract (to_bdd e) cube

  | ENot e -> Cudd.bddNot (to_bdd e)
  | EAnd (hd :: tl) -> List.fold_left Cudd.bddAnd (to_bdd hd) (List.map to_bdd tl)
  | EAnd [] -> Cudd.bddTrue()
  | EOr (hd :: tl) ->  List.fold_left Cudd.bddOr (to_bdd hd) (List.map to_bdd tl)
  | EOr [] -> Cudd.bddFalse()
    (*    | EXor (e,f) ->  Cudd.bddNot (Cudd.bddOr (Cudd.bddAnd (of_expr e) (of_expr f)) (Cudd.bddAnd (Cudd.bddNot (of_expr e)) (Cudd.bddNot (of_expr f))))
	  | EEqual (e,f) -> of_expr (ENot (EXor (e,f)))*)
  (* | EList el -> of_list (List.map to_bdd el)*)
  | True -> Cudd.bddTrue()
  | False -> Cudd.bddFalse()
