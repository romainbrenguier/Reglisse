
let display () =
  let t = Unix.times () in
  Printf.printf "[%.3fs]" t.tms_utime

let log string = 
  display ();
  print_endline string
