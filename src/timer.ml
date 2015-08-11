
let display () =
  let t = Unix.times () in
  Printf.printf "user    %fs\nsys     %fs\n" t.tms_utime t.tms_stime 
