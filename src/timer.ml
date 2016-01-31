let display_log = ref false
let display_warning = ref false

let display () =
  let t = Unix.times () in
  Printf.printf "[%.3fs]" t.tms_utime

let warning string =
  if !display_warning 
  then ( display (); Printf.printf " Warning: %s\n" string)

let debug string =
  if !Common.display_debug 
  then ( display (); Printf.printf " Debug: %s\n" string)

let log string = 
  if !display_log 
  then ( display (); Printf.printf " Log: %s\n" string )
