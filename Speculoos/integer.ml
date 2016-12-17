open Common
  
type t = Boolean.t array

let make array = array

let bool b = 
  if b then make [| Boolean.True |] 
  else make [| Boolean.False |] 

let of_array array = 
  Array.init (Array.length array) (fun i -> if array.(i) then Boolean.True else Boolean.False)

let to_boolean_array a = a

let int a = 
  let size = log a in
  let array = 
    Array.init size (fun i -> if a / (exp i) mod 2 = 0 then Boolean.False else Boolean.True)
  in array

(*let next a = Array.map Boolean.next a*)

let size a = Array.length (to_boolean_array a)

let get a j = 
  if j >= size a || j < 0 then Boolean.False else (to_boolean_array a).(j) 

let select array list =
  let size = 
    let rec aux s = function 
      | [] -> s
      | (a,b) :: tl -> if a >= b then aux (s+1+a-b) tl else aux (s+b+1-a) tl
    in aux 0 list
  in
  let tab = Array.make size Boolean.False in
  let pos = ref 0 in
  let rec aux = function 
    | [] -> ()
    | (a,b) :: tl ->
      if a >= b 
      then 
	for i = a downto b 
	do 
	  tab.(!pos) <- get array i;
	  incr pos
	done
      else
	for i = a to b 
	do 
	  tab.(!pos) <- get array i;
	  incr pos
	done;
      aux tl
  in  
  aux list;
  make tab


exception NonBoolean of t

let of_boolean b = make [| b |]
let to_boolean b = 
  if size b <> 1 then raise (NonBoolean b)
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

let var_name name i = name^"<"^string_of_int i^">"
let var name size = 
  make (Array.init size (fun i -> Boolean.var (var_name name i)))

let bool_var name = 
  of_boolean (Boolean.var name)

let size_max l = 
  let rec aux accu = function | [] -> accu | a :: l -> aux (max accu (Array.length a)) l
  in aux 0 l
      

let bitwise op a b = 
  let size = size_max [a;b] in
  let array = Array.init size 
    (fun i -> op (get a i) (get b i)) 
  in
  make array

let conj a b = bitwise Boolean.conj a b
let disj a b = bitwise Boolean.disj a b
let xor a b = bitwise Boolean.xor a b
let equiv a b = bitwise Boolean.equals a b
let implies a b = bitwise Boolean.implies a b

let bitwise_reduce op a = 
  Array.fold_left op a.(0) (Array.sub a 1 (size a - 1))

let andR = bitwise_reduce Boolean.conj
let orR = bitwise_reduce Boolean.disj
let xorR = bitwise_reduce Boolean.xor


let neg a = 
  let array = Array.init (size a)
    (fun i -> Boolean.neg (get a i))
  in make array

let not a = neg a
let (!!) a = neg a

let equals a b = 
  let size = size_max [a;b] in
  let expr = 
    iter 0 (size-1) (fun i ->  Boolean.conj (Boolean.equals (get a i) (get b i))) Boolean.True in
  expr


let add_1 a b c_in = 
  (Boolean.xor (Boolean.xor a b) c_in,
   Boolean.disj (Boolean.conj a b) 
     (Boolean.disj (Boolean.conj a c_in) 
	(Boolean.conj b c_in)))

let add a b = 
  let size = size_max [a; b] in
  let array = Array.make (size+1) (Boolean.False) in
  let _ = 
    iter 0 size 
      (fun i expr ->
	let e1 , e2 = add_1 (get a i) (get b i) expr in
	array.(i) <- e1; e2
      ) Boolean.False 
  in make array

let (++) a b = add a b

let ite c t e =
  let size = size_max [t ; e ]  in
  let a = Array.init size (fun i -> Boolean.disj (Boolean.conj c (get t i)) (Boolean.conj (Boolean.neg c) (get e i))) 
  in 
  make a
    
let for_each bounds f =
  let b = Boolean.for_each bounds (fun x -> to_boolean (f x)) in
  of_boolean b


let left_shift e i = 
  let size = max (size e + i) 1 in
  let a = Array.init size (fun j -> get e (j-i)) in
  make a

let right_shift e i = left_shift e (-i)
  
let mult a b = 
  let _ = size a + size b in
  let expr, nb =
    Array.fold_left 
      (fun (sum,i) ai -> 
	add sum (ite ai (left_shift b i) (int 0)), (i+1)
      ) ((int 0),0) (to_boolean_array a)
  in
  expr
    
let less a b =
  let size = size_max [a;b] in
  let expr = iter 0 (size - 1)
    (fun i accu -> 
      Boolean.conj
	(get b i) 
	(Boolean.disj (Boolean.neg (get a i))
	   (Boolean.conj
	      (Boolean.equals (get b i) (get a i))
	      accu
	   )
	)
    ) Boolean.False
  in expr
  
let greater a b = less b a
let less_eq a b = Boolean.neg (greater a b)
let greater_eq a b = Boolean.neg (less a b)

let solve_eq eq a = 
  let var_list = Array.to_list (to_boolean_array a) in
  let arr = Array.make (size a) Boolean.True in
  let rec aux accu list i = 
    match list with 
    | [] -> accu 
    | ai :: r ->
       (*Printf.printf "accu : %s\n\n" (Boolean.to_string accu);*)
       let b = Boolean.exists var_list (Boolean.conj ai eq) in
       (* Printf.printf "b : %s\n\n" (Boolean.to_string b);*)
       arr.(i) <- b;
       aux (Boolean.conj (Boolean.equals ai b) accu) r (i+1)
  in 
  let _ = aux eq var_list 0 in
  make arr

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
  let arr = make a in
  let sol = solve_eq eq arr in
  let nb,nlist = 
    List.fold_left 
      (fun (i,list) old -> 
	let b = Array.init size (fun j -> get sol (size * i + j)) in
	(i+1, make b :: list)
      ) (0,[]) list
  in
  List.rev nlist

let minus a b = 
  let size_c = size_max [a;b] in
  let c = var "_c" size_c in
  let eq = equals (add b c) a in
  let c_sol = solve_eq eq c in
  c_sol

let full_divide a b = 
  let size_d = size a in 
  let size_r = size b in
  let d = var "_d" size_d in 
  let r = var "_r" size_r in 
  let eq = Boolean.conj (less r b) (equals (add (mult d b) r) a) in
  match solve eq [d;r] with [d_sol; r_sol] -> d_sol , r_sol | _ -> failwith "wrong number of variables in full_divide"
    
let div a b = fst (full_divide a b)

let modulo a b = snd (full_divide a b)

let multiplex index array = 
  let nb, expr =
    Array.fold_left
      (fun (i,accu) expr ->
	i+1, ite (equals index (int i)) expr accu
      ) (0,of_array [|false|]) array
  in expr

