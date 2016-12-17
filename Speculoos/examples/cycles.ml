(* ./speculog.sh examples/cycles.ml *)

let aiger n = 
  let push = var "push" (Type.array Type.bool n) in
  let ca = var "controllable_a" (Type.array Type.bool n) in
  let cb = var "controllable_b" (Type.array Type.bool n) in
  let cycle = var "cycle" (Type.array (Type.int 3) n) in
  let cl = var "controllable_lamp" Type.bool in
  let err = var "err" Type.bool in
  
  let cycle_spec = 
    cycle, array
      (Array.init n 
	 (fun i -> 
	   (* if (cycle[i]==4 || (cycle[i] == 0 && !push)) then 0 else cycle + 1 *)
	   ite (disj (equals (get cycle (int i)) (int 4))
		  (conj (neg (get push (int i))) (equals (get cycle (int i)) (int 0))))
	     (int 0) (add (get cycle (int i)) (int 1))))
  in

  let cycle_error = 
    for_some [0,n-1] (fun i -> 
      disj 
	(neg (equiv (equals (get cycle (int i)) (int 2)) (get ca (int i))))
	(neg (equiv (equals (get cycle (int i)) (int 4)) (get cb (int i)))))
  in 
  let lamp_error = neg (equiv cl (for_some [0,n-1] (fun i -> greater (get cycle (int i)) (int 0)))) in

  let error_spec = err, disj cycle_error lamp_error in

  functional_synthesis [cycle_spec; error_spec]


let main = 
  Common.iter 1 100
    (fun i () -> 
     let file_name = "cycles_"^string_of_int i^".aag" in
     Printf.printf "writing aiger to %s\n" file_name;
     Aiger.write_to_file (aiger i) file_name
    ) ()
