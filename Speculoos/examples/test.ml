(* Synthesis of the aiger circuit. *)
let aiger = 
  (* Declaration of the variables *)
  output out 3;
  input inp 2;
  reg r 2;
  wire w 1;

  (* Specifications *)
  spec
    [
      next r === inp ;
      w.(0) == (r.(0) || inp.(0));
      out.(0) == inp.(0);
      out.(2) == not w.(0);
    ]



(* Write the generated aiger file on the standard output. *)
let main = Aiger.write_to_file aiger "test.aig"
