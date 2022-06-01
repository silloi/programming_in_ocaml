(* 3.7 (1) *)
let rec pow (x, n) = if n = 0 then 1. else x *. pow(x, n-1);;

(* 3.7 (2) *)
let rec pow x n =
  let m = n mod 2 in
  let d = n / 2 in
  if n = 0 then 1.
  else if n = 1 then x
  else pow (x ** 2.) d *. pow x m;;

(* 3.8 *)
let rec iterpow (i, res, n) =
  if i = n then res
  else iterpow (i + 1, res, n) * res;;

(* 3.9 *)
let cond (b, e1, e2) : int = if b then e1 else e2;;
(* val cond : bool * int * int -> int = <fun> *)
let rec fact n = cond ((n = 1), 1, n * fact (n-1));;
(* val fact : int -> int = <fun> *)
fact 4;;
(* Stack overflow during evaluation because n cannot be decremented *)

(* 3.10 *)
(*
  fib 4 -> let (i, _) = fib_pair 4 in i
        -> let (i, _) = if 4 = 1 then (1, 0) else let ('i, j) = fib_pair 3 in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = fib_pair 3 in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = if 3 = 1 then (1, 0) else let (''i, ''j) = fib_pair 2 in (''i + ''j, ''i) in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = let (''i, ''j) = fib_pair 2 in (''i + ''j, ''i) in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = let (''i, ''j) = if 2 = 1 then (1, 0) else let (''''i, ''''j) = fib_pair 1 in (''''i + ''''j, ''''i) in (''i + ''j, ''i) in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = let (''i, ''j) = let (''''i, ''''j) = fib_pair 1 in (''''i + ''''j, ''''i) in (''i + ''j, ''i) in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = let (''i, ''j) = let (''''i, ''''j) = if 1 = 1 then (1, 0) else let (''''''i, ''''''j) = fib_pair 1 in (''''''i + ''''''j, ''''''i) in (''''i + ''''j, ''''i) in (''i + ''j, ''i) in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = let (''i, ''j) = let (''''i, ''''j) = (1, 0) in (''''i + ''''j, ''''i) in (''i + ''j, ''i) in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = let (''i, ''j) = (1 + 0, 1) in (''i + ''j, ''i) in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = let (''i, ''j) = (1, 1) in (''i + ''j, ''i) in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = let (''i, ''j) = (1 + 1, 1) in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = let (''i, ''j) = (2, 1) in ('i + 'j, 'i) in i
        -> let (i, _) = let ('i, 'j) = (2 + 1, 1) in i
        -> let (i, _) = let ('i, 'j) = (3, 1) in i
        -> 3
*)

(* 3.11 (1) *)
let rec gcd (m, n) = if m = n then m else gcd(abs (n - m), m);;

(* 3.11 (2) *)
let rec comb (n, m) = if m = 0 || m = n then 1 else comb(n - 1, m) + comb(n - 1, m - 1);;

(* 3.11 (3) *)
let rec iterfib (i, res, n) = if i = 0 || i = 1 then res else iterfib(i - 1, res, n) + iterfib(i - 2, res, n);;

(* 3.11 (4) *)
let rec max_ascii (str, max_char) =
  if String.length str = 0 then max_char
  else let first_char = str.[0] in
    let rest_str = String.sub str 1 (String.length str - 1) in
      if int_of_char first_char > int_of_char max_char
        then max_ascii (rest_str, first_char)
        else max_ascii (rest_str, max_char);; 

(* 3.12 *)
let rec pos_neg n = if n < 0 then 0.0 else pos_neg (n - 1) +. 1.0 /. (float_of_int (4 * n + 1)) -. 1.0 /. (float_of_int (4 * n + 3));;
