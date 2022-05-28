(* 3.6 (1) *)
let geo_mean (x, y) =
  let s = x *. y in
    sqrt(s);;

(* 3.6 (2) *)
let bmi (name, height, weight) = 
  let i = weight /. height ** 2. in
    let status = if i < 18.5 then "やせています"
      else if i < 25. then "標準体型です"
      else if i < 30. then "肥満です"
      else "高度肥満です"
    in
      name ^ "さんは" ^ status;;

(* 3.6 (3) *)
let sum_and_diff (x, y) = (x + y, x - y);;
let retro_sum_and_diff (x, y) = ((x + y) / 2, (x - y) / 2);;
