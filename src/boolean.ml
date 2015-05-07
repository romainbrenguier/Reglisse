open Common

type t = 
| EVar of string * int
| ENext of string * int
| EExists of t list * t
| EForall of t list * t
| ENot of t
| EAnd of t list (* * t*)
| EOr of t list (* * t*)
| EList of t list
| True 
| False

let rec to_string = function 
  | EVar (s,i) -> Printf.sprintf "%s.(%d)" s i
  | ENext (s,i) -> Printf.sprintf "next %s.(%d)" s i
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
    
    
let var s i = EVar (s,i)
let next_var s i = ENext (s,i)
  
exception AlreadyNext of t
let rec next e = match e with
    | EVar (s,i) -> ENext (s,i)
    | ENext(_,_) -> raise (AlreadyNext e)
    | EExists (el,e) -> EExists (List.map next el, next e)
    | EForall (el,e) -> EForall (List.map next el, next e)
    | ENot x -> ENot (next x)
    | EAnd a -> EAnd (List.map next a)
    | EOr a -> EOr (List.map next a)
    | EList (el) -> EList (List.map next el)
    | True -> True
    | False -> False


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

let rec negation a = match a with
  | True -> False | False -> True 
  | ENot a -> a 
  | EOr a -> EAnd (List.map negation a)
  | EAnd a -> EOr (List.map negation a)
  | x -> ENot x
  
let disjunction a b = match a , b with
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
    | a, b -> if a = b then a else if a = negation b then True else EOr [a;b]

let conjunction a b = match a , b with
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
	
    
let xor a b = disjunction (conjunction a (negation b)) (conjunction (negation a) b)
  
let equality a b = disjunction (conjunction a b) (conjunction (negation a) (negation b))
  
let implication a b = disjunction b (negation a)
  
let for_each list f =
  let treat_one (start,last) =
    of_list (iter start last (fun i c -> (f i) :: c) [])
  in of_list (List.map treat_one list)
  
module BddMap = Map.Make(struct type t = Cudd.bdd let compare = Cudd.compare end)

let of_bdd bdd symbols = 
  let insert bdd expr map = 
    (*print_endline (to_string expr);*)
    let n_expr =
      try disjunction (BddMap.find bdd map) expr
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
    
  let aux bdd_expr_list (name,opt) = 
    let var = AigerBdd.Variable.find (name,opt) in
    BddMap.fold 
      (fun bdd expr accu->
	let var_bdd = AigerBdd.Variable.to_bdd var in
	let t = Cudd.bddRestrict bdd var_bdd in
	let expr_t = conjunction expr (EVar (name,opt)) in
        let e = Cudd.bddRestrict bdd (Cudd.bddNot var_bdd) in
	let expr_e = conjunction expr (ENot (EVar (name,opt))) in
	
	if Cudd.compare e t = 0
	then insert e expr accu 
	else
	  let accu = insert t expr_t accu in
	  let accu = insert e expr_e accu in
	  accu
      )  bdd_expr_list BddMap.empty
  in 
    
  let res = List.fold_left aux  (BddMap.add bdd True BddMap.empty) symbols (*[bdd,True]*) in
  try BddMap.find (Cudd.bddTrue()) res
  with _ -> failwith "in [of_bdd]: missing variables"
      


