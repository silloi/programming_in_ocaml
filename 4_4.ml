(* 4.1 *)
let uncurry f (x, y) = f x y;;

(* 4.2 *)
let rec repeat f n x = 
  if n > 0 then repeat f (n - 1) (f x) else x;;
(* let fib n =
  let (fibn, _) =  (i + j, i)
  in fibn;; *)

(* 4.3 *)
let ($) f g x = f (g x);;
let id x = x;;
let rec funny f n =
  if n = 0 then id
  else if n mod 2 = 0 then funny (f $ f) (n / 2)
  else funny (f $ f) (n / 2) $ f;;
(* 
  if n = 0 then identical
  else if n is even then do f twice with n/2
  else if n is odd then do f twice with n/2 and moreover f
*)

(* 4.4 *)
let s x y z = x z (y z);;
let k x y = x;;
let id = s k k;;
(* k 1 results to x -> 1 *)
(* s k (x -> 1) results to x -> 1 *)
(* s k k 1 results to s 1 (k 1) results to id (1) results to 1 *)

(* 4.5 *)
(* twice twice f x is twice f(f x) results to f( f( f( f x))))  *)

(* 4.6 *)
(* let f = k s k k s k k k;; *)
