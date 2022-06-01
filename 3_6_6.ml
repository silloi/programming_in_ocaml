(* 3.13 *)
let rec pow x n =
  let m = n mod 2 in
  let d = n / 2 in
  if n = 0 then 1.
  else if n = 1 then x
  else pow (x ** 2.) d *. pow x m;;
let cube x = pow x 3;; 

let rec pow n x =
  let m = n mod 2 in
  let d = n / 2 in
  if n = 0 then 1.
  else if n = 1 then x
  else pow d (x ** 2.) *. pow m x;;

let cube x = pow 3 x;;

(* 3.14 *)
let rec integral f a b = let del = 0.1e-10 in 
let rec iter_sum (i: float) (res: float) (n: float) = if i < a then res else 

(f (a +. (i -. 1.) *. del ) +. f (a +. i *. del) *. del) /. 2.
+. iter_sum (i -. del) res n
in iter_sum a 0. b;;

(* 3.15 *)
let int1_1_1_1 x y z = x + y + z;; (* int -> int -> int -> int *)
let int2_1_1 f x = f (x + 1) + f (x + 1);; (* (int -> int) -> int -> int *)
let int3_1 f = let add x = x + 1 in (f (add 1)) 2 + 3;; (* (int -> int -> int) -> int *)
