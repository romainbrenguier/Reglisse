let display_log = ref false

let display () =
  let t = Unix.times () in
  Printf.printf "[%.3fs]" t.tms_utime

let log string = 
  if !display_log 
  then
    (
      display ();
      print_endline string
    )
