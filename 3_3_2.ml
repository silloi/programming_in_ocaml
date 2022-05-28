(* 3.1 (1) *)
let yen_of_dollar d =
  let y = d *. 114.32 in floor y;;

(* 3.1 (1) *)
let dollar_of_yen y =
  let d = y /.114.32 in
    let d_times_100 = d *. 100. in
      let d_times_100_ceil = ceil d_times_100 in d_times_100_ceil /. 100.;;


(* 3.1 (3) *)
let dollars_are_yen d =
  let y = yen_of_dollar d in
    string_of_float d ^ " dollars are " ^ string_of_float y ^ " yen.";;

(* 3.1 (4) *)
let capitalize c =
  let i = int_of_char c in
    if 96 < i && i < 123 then char_of_int (i - 32) else c;;

(* 3.2 *)
let f_and b1 b2 = if b1 then if b2 then true else false else false;;
let f_and b1 b2 = if b1 then b2 else false;;
let f_or b1 b2 = if b1 then true else b2;;

(* 3.3 *)
let f_and b1 b2 = not (not b1 || not b2);;
let f_or b1 b2 = not (not b1 && not b2);;

(* 3.4 *)
let x = 1 in let x = 3 in let x = x + 2 in x * x;; (* = 25 *)
let x = 2 and y = 3 in (let y = x and x = y + 2 in x * y) + y;; (* = 13 *)
let x = 2 in let y = 3 in let y = x in let z = y + 2 in x * y * z;; (* = 16 *)

(* 3.5 *)
let pi = 3.1415926535;;
let e = 2.718281828;;
(* let _x = pi and _y = _x;; (* Unbound value _x *) *)
let _x = pi let _y = _x;;
