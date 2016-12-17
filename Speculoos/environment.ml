module Env = Map.Make(String)
type env = {modules: Aiger.t Env.t; parametric_modules: ParametricModule.t Env.t }

let empty_env = {modules=Env.empty; parametric_modules = Env.empty}
let add_module env s m = { env with modules = Env.add s m env.modules}
let find_module env s = 
  try Env.find s env.modules
  with Not_found -> Printf.eprintf "Module %s not found\n" s; raise Not_found

let merge_env m1 m2 = 
  let aux n m1 m2 = match m2 with
    | Some x -> m2 | None -> m1
  in 
  { modules=Env.merge aux m1.modules m2.modules; 
    parametric_modules = Env.merge aux m1.parametric_modules m2.parametric_modules }
  
let add_parametric_module env s m = { env with parametric_modules = Env.add s m env.parametric_modules}
let find_parametric_module env s = 
  try Env.find s env.parametric_modules
  with Not_found -> Printf.eprintf "Module %s not found\n" s; raise Not_found

let current_env = ref (fun () -> empty_env)
