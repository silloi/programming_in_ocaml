(* 3.7 (1) *)
let rec pow (x, n) = if n = 0 then 1. else x *. pow(x, n-1);;
(* let rec pow =  *)