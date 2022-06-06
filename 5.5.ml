(* 5.1 *)
[[]];; (* 'a list list *)
(* [[1; 3]; ["hoge"]] *)
[3] :: [];; (* int list list *)
(* 2 :: [3] :: [];; *)
[] :: [];; (* 'a list list *)
[(fun x -> x); (fun b -> not b)];; (* (bool -> bool) list*)

(* 5.2 (1) *)
let downto1 n =
  let rec downto1_rec i res n =
    if i = 0 then res
    else i :: downto1_rec (i - 1) res n
  in
  downto1_rec n [] n;;

downto1 6;;

(* 5.2 (2) *)
let rec roman al n = match al with
    [] -> ""
  | (u, s) :: rest -> let flg =  n / u in
    if flg > 0 then s ^ roman al (n - u)
    else roman rest n;;

roman [(1000, "M"); (500, "D"); (100, "C"); (50, "L");
    (10, "X"); (5, "V"); (1, "I")] 1984;;

roman [(1000, "M"); (900, "CM"); (500, "D"); (400, "CD");
    (100, "C"); (90, "XC"); (50, "L"); (40, "XL");
    (10, "X"); (9, "IX"); (5, "V"); (4, "IV"); (1, "I")] 1984;;

(* 5.2 (3) *)
let rec nested_length l =
  let rec length = function
      [] -> 0
    | x :: rest -> 1 + length rest
  in
  match l with
    [] -> 0
  | x :: rest -> match x with
      [] -> nested_length rest
    | _ -> length x + nested_length rest;;

nested_length [[1; 2; 3]; [4; 5]; [6]; [7; 8; 9; 10]];;

(* 5.2 (4) *)
let rec concat l = match l with
    [] -> []
  | x :: rest -> match x with
      [] -> concat rest
    | l' -> l' @ concat rest;;

concat [[0; 3; 4]; [2]; []; [5; 0]];;

(* 5.2 (5) *)
let rec zip a b = match (a, b) with
    ([], []) -> []
  | (ai :: arest, bi :: brest) -> (ai, bi) :: zip arest brest
  | (_, _) -> [];;

zip [2; 3; 4; 5; 6; 7; 8; 9; 10; 11]
  [true; true; false; true; false; true; false; false; false; true];;

(* 5.2 (6) *)
let rec unzip = function
    [] -> ([], [])
  | (ai, bi) :: rest -> let (ar, br) = unzip rest in (ai :: ar, bi :: br);;

unzip (zip [2; 3; 4; 5; 6; 7; 8; 9; 10; 11]
  [true; true; false; true; false; true;
  false; false; false; true]);;

(* 5.2 (7) *)
let rec filter f = function
    [] -> []
  | x :: rest when f x -> x :: filter f rest
  | x :: rest -> filter f rest;;

let is_positive x = (x > 0);;
filter is_positive [-9; 0; 2; 5; -3];;

let rec length = function
      [] -> 0
    | x :: rest -> 1 + length rest;;
filter (fun l -> length l = 3) [[1; 2; 3]; [4; 5]; [6; 7; 8]; [9]];;

(* 5.2 (8) *)
let rec take n = function
    [] -> []
  | x :: rest when n > 0 -> x :: take (n - 1) rest
  | _ -> [];;

let rec drop n = function
    [] -> []
  | x :: rest when n > 0 -> drop (n - 1) rest
  | l -> l;; 

let ten_to_zero = downto1 10;;
take 8 ten_to_zero;;
drop 7 ten_to_zero;;

(* 5.2 (9) *)
let max_list l =
  let rec max_list_rec l' max_num =
    match l' with
        [] -> max_num
      | x :: rest when x > max_num -> max_list_rec rest x
      | x :: rest -> max_list_rec rest max_num
  in max_list_rec l min_int;;

max_list [7; 9; 0; -5];;

(* 5.3 (1) *)
let rec mem a s = match s with
    [] -> false
  | v :: rest -> if v = a then true else mem a rest;;

(* 5.3 (2) *)
let rec intersect s1 s2 = 
  let (s1, s2) = if length s2 > length s1 then (s2, s1) else (s1, s2) in
  match s1 with
    [] -> []
  | s1v :: s1rest when mem s1v s2 -> s1v :: intersect s1rest s2
  | _ :: s1rest -> intersect s1rest s2;;

(* 5.3 (3) *)
let union s1 s2 = let s1s2 = s1 @ s2 in
  let rec uniq s = match s with
      [] -> []
    | x :: rest when mem x rest -> uniq rest
    | x :: rest -> x :: uniq rest
  in uniq s1s2;;

let rec union2 s1 s2 = 
  let (s1, s2) = if length s2 > length s1 then (s2, s1) else (s1, s2) in
  match s1 with
      [] -> []
    | s1v :: s1rest when mem s1v s2 -> union2 s1rest s2
    | s1v :: s1rest -> s1v :: union2 s1rest s2;;

(* 5.3 (4) *)
let diff s1 s2 = 
  let (s1, s2) = if length s2 > length s1 then (s2, s1) else (s1, s2) in
  let rec diff_rec l1 l2 =
  match l1 with
      [] -> []
    | l1v :: l1rest when mem l1v l2 -> diff_rec l1rest l2
    | l1v :: l1rest -> l1v :: diff_rec l1rest l2 in
  diff_rec s1 s2;;

(* 5.4 *)
let rec map f = function
    [] -> []
  | x :: rest -> f x :: map f rest;;

let f x = x * 2;;
let g x = x + 1;;

map f (map g [1; 2; 3]);;
map (fun y -> let x = g y in f x) [1; 2; 3];;

(* 5.5 *)
let rec fold_right f l e = 
  match l with
      [] -> e
    | x :: rest -> f x (fold_right f rest e);;

let concat l = let f ll x = ll @ x
in fold_right f l [];;

let forall p l = let f x y = (p x) && y in fold_right f l true;;

let exists p l = let f x y = (p x) || y in fold_right f l false;;

(* 5.6 *)
(* let rec quick_sort = function
        ([] | [_]) as l -> l
      | pivot :: rest ->
        let rec partition left right sorted = function
              [] -> quick_sort sorted
          | y :: ys -> if pivot < y then partition left (y :: right) ys sorted
                          else partition (y :: left) right ys sorted
in partition [] [] [] rest;; *)


(* 5.7 *)
(* let squares r =
  let sqrt_r = sqrt (float_of_int r) in
    let xlist =  *)

(* 5.8 *)
(* let map2 f = function
    [] -> []
  | x :: rest ->  *)
