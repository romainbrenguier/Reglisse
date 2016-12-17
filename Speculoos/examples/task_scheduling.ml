open Speculog
open Expr
open Util

let consumption1 = 4
let consumption2 = 3
let consumption_bus = 10
let idle_consumption1 = 1
let idle_consumption2 = 1
let idle_consumption_bus = 1
let execution_time_1_1 = 1
let execution_time_1_2 = 1
let execution_time_2_1 = 2
let execution_time_2_2 = 2
let execution_time_3_1 = 11
let execution_time_3_2 = 7
let communication_time_1 = 8
let communication_time_2 = 4

let aiger =
  (* 0 means idle; 1 computing task 1; 2 computing task 2 *)
  reg p1 2 in
    reg p2 2 in
    reg bus 2 in
  reg computed1 (log (max execution_time_1_1 execution_time_1_2)) in
  reg computed2 (log (max execution_time_2_1 execution_time_2_2)) in
  reg computed3 (log (max execution_time_3_1 execution_time_3_2)) in
  
  (* tells on which processors we should start computing; 3 means on the bus *)
  input start1 2 in
  input start2 2 in
  input start3 2 in

  spec
    [
      ite (start1 === int 1) (next p1 === int 1) (True);
      ite (start1 === int 2) (next p2 === int 1) (True);
    ]


let main = Aiger.write aiger stdout
