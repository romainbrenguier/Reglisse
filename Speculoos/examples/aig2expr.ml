(* ocamlbuild -tag use_ocaml-aiger -tag use_ocaml-cudd examples/aig2expr.byte *)

let aig = 
  try Aiger.read_from_file Sys.argv.(1)
  with _ -> failwith ("usage: "^Sys.argv.(0)^" aiger_file.aig")

let _ = AigerBdd.init aig 
let _ = print_endline Sys.argv.(1)

let aigBdd = AigerBdd.of_aiger aig 

let output = match aig.Aiger.outputs with 
  | [] -> failwith "no output"
  | o :: [] -> o
  | _ :: _ :: tl -> failwith "more than one output"
    
let symbols = 
  List.fold_left
    (fun a i -> 
      (AigerBdd.of_aiger_symbol (Aiger.lit2symbol aig i)) :: a
    ) [] aig.Aiger.inputs

let symbols = 
  List.fold_left 
    (fun a (l,_) ->
      (AigerBdd.of_aiger_symbol (Aiger.lit2symbol aig l)) :: a
    ) symbols aig.Aiger.latches


module BddSet = Set.Make (struct type t = Cudd.bdd let compare = Cudd.compare end)


let nb_var_in_bdd bdd =
  let var = Hashtbl.create 100 in
  let rec aux visited n = 
    if BddSet.mem n visited then visited
    else if Cudd.isConstant n then visited 
    else
      let i = Cudd.nodeReadIndex n in
      Hashtbl.replace var i true;
      let visited = aux visited (Cudd.t n) in
      let visited = aux visited (Cudd.e n) in
      BddSet.add n visited
  in
  let _ = aux BddSet.empty bdd in
  Hashtbl.length var  



(* split a bdd in two such that the intersection of the two correspond to the argument and the first bdd does not depend on the given latches *)
let split bdd latches = 
  let independent = Cudd.bddExistAbstract bdd (AigerBdd.Variable.make_cube latches) in
  let over_approx = Cudd.bddRestrict bdd independent in
  independent, over_approx

let threshold = 20


let add_to_conjunction bdd (set,conj) =
  let c = Cudd.bddAnd conj bdd in
  if Cudd.equal c conj
  then (print_endline "not adding to the conjunction"; (set,conj))
  else BddSet.add bdd set, c



let algo1 err =
  Cudd.dumpDot "err.dot" err;
  print_endline "wrote err.dot";
  Printf.printf "size of err : %d / nb latches : %d / nb variables : %d\n" (Cudd.dagSize err) (List.length aig.Aiger.latches) (nb_var_in_bdd err);
  print_newline ();

  let rec loop (to_keep,to_refine) i = function 
    | a :: tl ->
       let tk,tr =
	 BddSet.fold 
	   (fun bdd (tk,tr) ->
	    let ind,ovr = split bdd [a] in
	    Printf.printf "bdd : %d %d; ind : %d %d; ovr : %d %d\n" (Cudd.dagSize bdd) (nb_var_in_bdd bdd) (Cudd.dagSize ind) (nb_var_in_bdd ind) (Cudd.dagSize ovr) (nb_var_in_bdd ovr);

	    let tk,tr = 
	      if (nb_var_in_bdd ind) < threshold 
	      then add_to_conjunction ind tk, tr
	      else tk, add_to_conjunction ind tr 
	    in
	    let tk,tr =
	      if (nb_var_in_bdd ovr) < threshold
	      then add_to_conjunction ovr tk,tr
	      else tk, add_to_conjunction ovr tr
	    in tk,tr
	   ) (fst to_refine) (to_keep,(BddSet.empty,Cudd.bddTrue ()) )
       in
       loop (tk,tr) (i+1) tl
	    
    | [] -> 
       let rem = BddSet.union (fst to_refine) (fst to_keep) in
       (* let non_trivial = BddSet.filter (fun x -> not (Cudd.equal (Cudd.bddTrue()) x)) rem in*)
       Printf.printf "remaning : %d bdds\n" (BddSet.cardinal rem);
       BddSet.fold
	 (fun bdd i ->
	  Printf.printf "writing bdd_%d.dot of size:%d with %d variables\n" i (Cudd.dagSize bdd) (nb_var_in_bdd bdd);
	  Cudd.dumpDot ("bdd_"^string_of_int i^".dot") bdd;
	  i+1
	 ) rem 0 
		     
  in
  loop ((BddSet.empty,Cudd.bddTrue()),(BddSet.singleton (Cudd.bddNot err),Cudd.bddNot err)) 0 (List.map (fun (l,_) -> AigerBdd.Variable.of_lit aig l) aig.Aiger.latches)



let display_bdd_set bdd_set =   
  Printf.printf "numbers of BDDs: %d\n" (BddSet.cardinal bdd_set);
  BddSet.fold
    (fun bdd i ->
     (*Printf.printf "writing bdd%d.dot of size:%d with %d variables\n" i (Cudd.dagSize bdd) (nb_var_in_bdd bdd);
     Cudd.dumpDot (Printf.sprintf "bdd%d.dot" i) bdd;*)
     Printf.printf " %d " (nb_var_in_bdd bdd);
     i+1
    ) bdd_set 0;
  print_newline ()


let algo2 err = 
  Printf.printf "size of err : %d / nb latches : %d / nb variables : %d\n" (Cudd.dagSize err) (List.length aig.Aiger.latches) (nb_var_in_bdd err);

  let rec loop res (bdd1 , bdd2) = function
    | hd :: tl ->
       let (b1,b2) = 
	 List.fold_left 
	   (fun (bdd1,bdd2) var ->
	    let a1 = Cudd.bddExistAbstract bdd1 (AigerBdd.Variable.make_cube [hd]) in
	    if Cudd.equal a1 bdd1 then (bdd1,bdd2)
	    else 
	      let a2 = Cudd.bddExistAbstract bdd2 (AigerBdd.Variable.make_cube [hd]) in
	      if Cudd.equal a2 bdd2 then (bdd1,bdd2)
	      else
		let b1 = Cudd.bddExistAbstract bdd2 (AigerBdd.Variable.make_cube [var]) in
		if Cudd.equal b1 bdd2 then (bdd1,bdd2)
		else
		  let c1 = Cudd.bddAnd b1 a1 in
		  if Cudd.equal c1 res
		  then 
		    (
		      Printf.printf "a1(%d) b1(%d) is good enough\n" (nb_var_in_bdd a1) (nb_var_in_bdd b1);
		      a1,b1
		    )
		  else
		    let b2 = Cudd.bddExistAbstract bdd1 (AigerBdd.Variable.make_cube [var]) in
		    if Cudd.equal b2 bdd1 then (bdd1,bdd2)
		    else
		      let c2 = Cudd.bddAnd b2 a2 in
		      if Cudd.equal c2 res
		      then 
			(
			  print_endline "a2 b2 is good enough!";
			  (a2,b2)
			)
		      else 
			(
			  (* print_endline "cannot abstract";*)
			  (bdd1,bdd2)
			)
	   ) (bdd1,bdd2) tl
       in loop res (b1,b2) tl
    | [] -> 
       bdd1,bdd2
  in 

  let rec aux (to_keep,to_refine) = 
    if BddSet.is_empty to_refine
    then to_keep
    else
      (
	let tk,tr =
	  BddSet.fold 
	    (fun  bdd (to_keep,to_refine) ->
	     Printf.printf "refining bdd of size:%d with %d variables\n" (Cudd.dagSize bdd) (nb_var_in_bdd bdd);
	     let bdd1,bdd2 = loop bdd (bdd,bdd) (List.map (fun (l,_) -> AigerBdd.Variable.of_lit aig l) aig.Aiger.latches) in
	     if Cudd.equal bdd1 bdd || Cudd.equal bdd2 bdd
	     then (BddSet.add bdd to_keep,to_refine)
	     else
	       let tk,tr =
		 if nb_var_in_bdd bdd1 > threshold 
		 then (to_keep,BddSet.add bdd1 to_refine)
		 else (BddSet.add bdd1 to_keep, to_refine)
	       in 
	       let tk,tr =
		 if nb_var_in_bdd bdd2 > threshold
		 then (tk,BddSet.add bdd2 tr)
		 else (BddSet.add bdd2 tk, tr)
	       in tk,tr
	    ) to_refine (to_keep,BddSet.empty) 
	in
	print_endline "refining -------"; 
	aux (tk,tr)
      )
	     

  in
  let bdd_set = aux (BddSet.empty,BddSet.singleton err) in

  let remove_useless set =
    fst (BddSet.fold 
	   (fun bdd (s,set) ->
	    let conj = BddSet.fold Cudd.bddAnd set (Cudd.bddTrue()) in
	    let c = BddSet.fold Cudd.bddAnd (BddSet.remove bdd set) (Cudd.bddTrue()) in
	    if Cudd.equal c conj then (s,set) else BddSet.add bdd s,BddSet.remove bdd set
	   ) set (BddSet.empty,set))
  in
  
  let bdd_set = remove_useless bdd_set in
  display_bdd_set bdd_set
  


let pairs list = 
  let rec aux accu = function
    | [] -> accu
    | hd :: tl -> aux (List.fold_left (fun accu x -> (hd,x) :: accu) accu tl) tl
  in aux [] list


let algo3 err = 
  Printf.printf "size of err : %d / nb latches : %d / nb variables : %d" (Cudd.dagSize err) (List.length aig.Aiger.latches) (nb_var_in_bdd err);
  print_newline ();

  let latches = List.map (fun (l,_) -> AigerBdd.Variable.of_lit aig l) aig.Aiger.latches in

  let rec dec bdd list =
    Printf.printf "list size : %d \n" (List.length list);
    if List.length list < List.length latches - 5 then BddSet.singleton bdd
    else
    let rec aux = function 
      | (l1,l2) :: tl ->
	 let a1 = Cudd.bddUnivAbstract bdd (AigerBdd.Variable.make_cube [l1]) in
	 if Cudd.equal a1 bdd 
	 then dec a1 (List.filter (fun x -> x <> l1) list)
	 else 
	   let a2 = Cudd.bddUnivAbstract bdd (AigerBdd.Variable.make_cube [l2]) in
	   if Cudd.equal a2 bdd 
	   then dec a2 (List.filter (fun x -> x <> l2) list)
	   else 
	     if Cudd.equal (Cudd.bddOr a1 a2) bdd
	     then 
	       let set1 = dec a1 (List.filter (fun x -> x <> l1) list) in
	       let set2 = dec a2 (List.filter (fun x -> x <> l2) list) in
	       BddSet.union set1 set2
		 
	     else aux tl
      | [] -> BddSet.singleton bdd
    in
    aux (pairs list)
  in 
  let bdd_set = dec err latches in
  display_bdd_set bdd_set    



let main = 
  let var = AigerBdd.Variable.of_lit aig output in
  let err = Hashtbl.find (AigerBdd.updates aigBdd) var in
  algo3 err
  

(*  let inter = Cudd.bddAnd over indep in
  Cudd.dumpDot "inter.dot" over;
  print_endline "wrote inter.dot";

  if Cudd.equal bdd inter 
  then print_endline "intersection correct"
  else print_endline "intersection NOT correct"
 *)
  (*print_endline "converting BDD to expression...";*)
  (*let expr = Boolean.of_bdd bdd symbols in
  print_endline (Boolean.to_string expr)
   *)



