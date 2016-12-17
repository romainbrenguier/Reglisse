open Speculoos

let output_file = ref ""
let input_file = ref ""

let arguments = 
  let open Arg in
  [ "-o", Set_string output_file, "Output the generated circuit in the given aiger file"]

let set_input_file f = input_file := f

let parse_arguments =
  Arg.parse arguments set_input_file ("usage: "^Sys.argv.(0)^" <options> file.spec")

    
let main = 
  let inch = open_in !input_file in
  let spec = Parser.parse_inch inch in
  close_in inch;
  Cudd.init 30;
  if !output_file <> "" 
  then Speculoos.compile ~filename:!output_file spec 
  else Speculoos.compile spec;
  Cudd.quit ()


