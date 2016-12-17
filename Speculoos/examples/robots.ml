(* This file requieres Bigarray, to compile it use: 
   ./speculog.sh examples/robots.ml  "-tag use_bigarray"*)
open Expression

let init_x = 10
let init_y = 25

(* use a format described in http://wiki.ros.org/map_server for describing the problem ? *)


let read_ppm filename =
  let inch = open_in filename in
  let line = input_line inch in
  if line <> "P6" then invalid_arg "not a P6 ppm file";
  let line = input_line inch in
  let line =
    try if line.[0] = '#' 
      then input_line inch
      else line
    with _ -> line
  in
  let width, height = Scanf.sscanf line "%d %d" (fun w h -> (w, h)) in
  let line = input_line inch in
  if line <> "255" then invalid_arg "not a 8 bit depth image";
  let all_channels =
    let kind = Bigarray.int8_unsigned
    and layout = Bigarray.c_layout
    in
    Bigarray.Array3.create kind layout 3 width height
  in
  let r_channel = Bigarray.Array3.slice_left_2 all_channels 0
  and g_channel = Bigarray.Array3.slice_left_2 all_channels 1
  and b_channel = Bigarray.Array3.slice_left_2 all_channels 2
  in
  for y = 0 to pred height do
    for x = 0 to pred width do
      r_channel.{x,y} <- (input_byte inch);
      g_channel.{x,y} <- (input_byte inch);
      b_channel.{x,y} <- (input_byte inch);
    done;
  done;
  close_in inch;
  (all_channels,
   r_channel,
   g_channel,
   b_channel)

let map,size_x,size_y =
  let (a,r,g,b) = read_ppm "examples/map.ppm" in
  let size_x = Bigarray.Array2.dim1 r in
  let size_y = Bigarray.Array2.dim2 r in
  print_endline "Map taken from examples/map.ppm:";
  for i = 0 to size_x - 1 do
    for j = 0 to size_y - 1 do
      Printf.printf "%d" (r.{i,j}/200)
    done;
    print_newline ()
  done;
  r,size_x,size_y


let aiger = 
  let x = var "x" (Type.int 8) in
  let y = var "y" (Type.int 8) in
  (* 0 means move to the left; 1 don't move on x; 2 move to the right *)
  let dx = var "controllable_dx" (Type.int 2) in
  let dy = var "controllable_dy" (Type.int 2) in
  let fail = var "fail" Type.bool in
  let spec = 
    [
      x, minus (x $+ dx) (int 1);
      y, minus (add y dy) (int 1);
      fail, disj (disj (equals dx (int 3)) (equals dy (int 3)))
		 (for_some
		    [0,size_x-1] 
		    (fun i ->
		     (for_some
			[0,size_y-1] 
			(fun j ->
			 if map.{i,j} < 200
			 then 
			   ite (conj (equals x (int i)) (equals y (int j))) (bool true) (bool false)
			 else bool false
		     ))))
    ]
  in 
  let init_spec = init [x, int init_x; y, int init_y] spec in
  functional_synthesis init_spec 
	     

let main = 
  print_endline "writing aiger to robots.aag";
  Aiger.write_to_file aiger "robots.aag"

