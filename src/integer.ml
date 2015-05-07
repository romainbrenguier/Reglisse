open Common

type 'a t = ('a option * Boolean.t array)

type s_bool = bool t
type s_int = int t

let make ao array = (ao,array)
let cast ((fake,array):'a t) = make None array

let bool b = 
  if b
  then make None [| Boolean.True |] 
  else make None [| Boolean.False |] 


let of_array array = 
  (None,Array.init (Array.length array) (fun i -> if array.(i) then Boolean.True else Boolean.False))



let int a = 
  let size = log a in
  let array = 
    Array.init size (fun i -> if a / (exp i) mod 2 = 0 then Boolean.False else Boolean.True)
  in make (*Some a*) None array

let next a = (fst a,Array.map Boolean.next (snd a))

let to_boolean_array a = snd a
let size a = Array.length (to_boolean_array a)

let get a j = 
  if j >= size a || j < 0 then Boolean.False else (to_boolean_array a).(j) 

exception NonBoolean of (unit t)

let of_boolean b = make None [| b |]
let to_boolean b = 
  if size b <> 1 then raise (NonBoolean (cast b))
  else (to_boolean_array b).(0)

let to_string b = 
  if size b = 1 then Boolean.to_string (to_boolean b)
  else
    let buf = Buffer.create 100 in
    Array.iter
      (fun bi -> 
       Printf.bprintf buf "%s;" (Boolean.to_string bi) 
      ) (to_boolean_array b);
    Buffer.contents buf

let get_expr a j = of_boolean (get a j) 

let var name size = 
  make None (Array.init size (fun i -> Boolean.var name i))


let size_max l = 
  let rec aux accu = function | [] -> accu | a :: l -> aux (max accu (Array.length (snd a))) l
  in aux 0 l
      

let bitwise op a b = 
  let size = size_max [a;b] in
  let array = Array.init size 
    (fun i -> op (get a i) (get b i)) 
  in
  make (fst a) array

let ( &&) a b = bitwise Boolean.conjunction a b
let conj a b = bitwise Boolean.conjunction a b
let ( or ) a b = bitwise Boolean.disjunction a b
let disj a b = bitwise Boolean.disjunction a b
let xor a b = bitwise Boolean.xor a b
let (^^) a b = xor a b
let equiv a b = bitwise Boolean.equality a b
let implies a b = bitwise Boolean.implication a b


let neg a = 
  let array = Array.init (Array.length (snd a)) 
    (fun i -> Boolean.negation (get a i))
  in make (fst a) array

let not a = neg a
let (!!) a = neg a

let equals a b = 
  let size = size_max [a;b] in
  let expr = 
    iter 0 (size-1) (fun i ->  Boolean.conjunction (Boolean.equality (get a i) (get b i))) Boolean.True in
  of_boolean expr

let ( == ) a b = equals a b

let add_1 a b c_in = 
  (Boolean.xor (Boolean.xor a b) c_in,
   Boolean.disjunction (Boolean.conjunction a b) 
     (Boolean.disjunction (Boolean.conjunction a c_in) 
	(Boolean.conjunction b c_in)))

let add a b = 
  let size = size_max [a; b] in
  let array = Array.make (size+1) (Boolean.False) in
  let _ = 
    iter 0 size 
      (fun i expr ->
	let e1 , e2 = add_1 (get a i) (get b i) expr in
	array.(i) <- e1; e2
      ) Boolean.False 
  in make (fst a) array

let (++) a b = add a b

let ite i t e =
  let size = size_max [t ; e ]  in
  let boolean = to_boolean i in
  let condition = make None (Array.make size boolean) in
  ((condition && t) or ((not condition) && e))
    
let for_each bounds f =
  let b = Boolean.for_each bounds (fun x -> to_boolean (f x)) in
  of_boolean b


let left_shift e i = 
  let size = max (size e + i) 1 in
  let a = Array.init size (fun j -> get e (j-i)) in
  make (fst e) a

let right_shift e i = left_shift e (-i)
  
let mult a b = 
  let _ = size a + size b in
  let expr, nb =
    Array.fold_left 
      (fun (sum,i) ai -> 
	add sum (ite (of_boolean ai) (left_shift b i) (cast (int 0))), (i+1)
      ) (cast (int 0),0) (to_boolean_array a)
  in
  expr
    
let ( ** ) a b = mult a b

let less a b =
  let size = size_max [a;b] in
  let expr = iter 0 (size - 1)
    (fun i accu -> 
      ((get_expr b i) && (!! (get_expr a i))) or (((get_expr b i) == (get_expr a i)) && accu)
    ) (bool false)
  in expr
  
let greater a b = less b a
let less_eq a b = !! (greater a b)
let greater_eq a b = !! (less a b)

let solve_eq eq a = 
  let var_list = Array.to_list (to_boolean_array a) in
  let eq = to_boolean eq in
  let arr = Array.make (size a) Boolean.True in
  let rec aux accu list i = 
    match list with 
    | [] -> accu 
    | ai :: r ->
       (*Printf.printf "accu : %s\n\n" (Boolean.to_string accu);*)
       let b = Boolean.exists var_list (Boolean.conjunction ai eq) in
       (* Printf.printf "b : %s\n\n" (Boolean.to_string b);*)
       arr.(i) <- b;
       aux (Boolean.conjunction (Boolean.equality ai b) accu) r (i+1)
  in 
  let _ = aux eq var_list 0 in
  make None arr

let solve eq list = 
  let size = size_max list in
  let a = Array.make (size * List.length list) Boolean.False in
  let _ = 
    List.fold_left 
      (fun i b -> 
	for j = 0 to size - 1 do
	  a.(size * i + j) <- get b j 
	done;
	i+1
      ) 0 list
  in
  let arr = make None a in
  let sol = solve_eq eq arr in
  let nb,nlist = 
    List.fold_left 
      (fun (i,list) old -> 
	let b = Array.init size (fun j -> get sol (size * i + j)) in
	(i+1, make None b :: list)
      ) (0,[]) list
  in
  List.rev nlist

let minus a b = 
  let size_c = size_max [a;b] in
  let c = var "_c" size_c in
  let eq = ((add b c) == a) in
  let c_sol = solve_eq eq c in
  c_sol

let ( -- ) a b = minus a b
  
let full_divide a b = 
  let size_d = size a in 
  let size_r = size b in
  let d = var "_d" size_d in 
  let r = var "_r" size_r in 
  let eq = (less r b) && ((add (mult d b) r) == a) in
  match solve eq [d;r] with [d_sol; r_sol] -> d_sol , r_sol | _ -> failwith "wrong number of variables in full_divide"
    
let div a b = fst (full_divide a b)

let modulo a b = snd (full_divide a b)

let multiplex index array = 
  let nb, expr =
    Array.fold_left
      (fun (i,accu) expr ->
	i+1, ite (equals index (cast (int i))) expr accu
      ) (0,of_array [|false|]) array
  in expr

