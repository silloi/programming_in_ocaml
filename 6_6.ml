(* 6.1 *)
type figure =
    Point
  | Circle of int
  | Rectangle of int * int
  | Square of int;;

let similar x y =
  match (x, y) with
      (Point, Point) | (Circle _, Circle _) | (Square _, Square _) -> true
    | (Rectangle (l1, l2), Square(_)) | (Square(_), Rectangle(l1, l2)) when l1 = l2 -> true
    | (Rectangle (l1, l2), Rectangle (l3, l4)) -> (l3 * l2 - l4 * l1) = 0
    | _ -> false;;

(* 6.2 *)
type 'a with_location = {loc_x: float; loc_y: float; body: 'a};;

let overlap x y =
  match (x, y) with
      ({loc_x = x_loc_x; loc_y = x_loc_y; body = Point}, {loc_x = y_loc_x; loc_y = y_loc_y; body = Point}) -> x_loc_x = y_loc_x && y_loc_x = y_loc_y
    | ({loc_x = x_loc_x; loc_y = x_loc_y; body = Circle x_body}, {loc_x = y_loc_x; loc_y = y_loc_y; body = Circle y_body}) -> (x_loc_x -. y_loc_x) ** 2. +. (y_loc_x -. y_loc_y) ** 2. > float_of_int x_body ** 2. +. float_of_int y_body ** 2.
    | ({loc_x = x_loc_x; loc_y = x_loc_y; body = Rectangle (x_l1, x_l2)}, {loc_x = y_loc_x; loc_y = y_loc_y; body = Rectangle (y_l1, y_l2)}) -> abs (x_l1 - y_l1) / 2 < int_of_float (abs_float (x_loc_x -. y_loc_x)) && abs (x_l2 - y_l2) / 2 < int_of_float (abs_float (y_loc_x -. y_loc_y))
    | ({loc_x = x_loc_x; loc_y = x_loc_y; body = Square x_body}, {loc_x = y_loc_x; loc_y = y_loc_y; body = Square y_body}) -> abs (x_body - y_body) / 2 < int_of_float (abs_float (x_loc_x -. y_loc_x)) && abs (x_body - y_body) / 2 < int_of_float (abs_float (y_loc_x -. y_loc_y))
    | _ -> false;; (* not impmelented *)

(* 6.3 *)
type nat = Zero | OneMoreThan of nat;;

let rec add m n =
  match m with Zero -> n | OneMoreThan m' -> OneMoreThan (add m' n);;

let rec mul m n = match m with
      Zero -> Zero
    | OneMoreThan m' -> add n (mul m' n);;

let rec monus m n = match m with
    Zero -> Zero
  | OneMoreThan m' -> match n with
        Zero -> m
      | OneMoreThan n' -> monus m' n';;

(* 6.4 *)
let rec minus m n = match m with
    Zero -> begin
        match n with
        Zero -> Some(Zero)
      | OneMoreThan _ -> None
    end
  | OneMoreThan m' -> match n with
        Zero -> Some(m)
      | OneMoreThan n' -> minus m' n';;

(* 6.5 *)
type 'a tree = Lf | Br of 'a * 'a tree * 'a tree;;

let rec comptree x = function
    0 -> Lf
  | n -> Br (x, comptree x (n - 1), comptree x (n - 1));;

let comptree' n = 
  let rec comptree'_iter label ini fin = 
    if ini = fin then Lf
    else Br (label, comptree'_iter (label * 2 + 1) (ini + 1) fin, comptree'_iter (label * 2) (ini + 1) fin)
  in comptree'_iter 1 0 n;;

(* 6.6 *)
let rec preord t l =
    match t with
      Lf -> l
    | Br(x, left, right) -> x :: (preord left (preord right l));;

let rec inord t l =
    match t with
      Lf -> l
    | Br(x, left, right) -> inord left (x :: inord right l);;

let rec postord t l =
  match t with
    Lf -> l
  | Br(x, left, right) -> postord left (postord right (x :: l));;
  

(* 6.7 *)
let rec reflect t = match t with
    Lf -> Lf
  | Br (x, left, right) -> Br (x, reflect right, reflect left);;

(* preorder(reflect(t)) = rev(postorder(t));; *)
(* inorder(reflect(t)) = rev(inorder(t));; *)
(* postorder(reflect(t)) = rev(preorder(t));; *)

(* 6.8 *)
type 'a rosetree = RLf | RBr of 'a * 'a rosetree list;;

let rtree =
RBr ("a",
  [RBr ("b", [RBr ("c", [RLf]); RLf; RBr ("d", [RLf])]); RBr ("e", [RLf]);
    RBr ("f", [RLf])]);;

let rec tree_of_rtree = function
    RLf -> Br (None, Lf, Lf)
    | RBr (a, rtrees) -> Br (Some a, tree_of_rtreelist rtrees, Lf)
and tree_of_rtreelist = function
    [] -> Lf
  | rtree :: rest -> let Br (a, left, Lf) = tree_of_rtree rtree in
Br(a, left, tree_of_rtreelist rest);;

(* let rec rtree_of_tree = function
    Lf -> RLf
  | Br (Some(x), left, right) -> RBr (x, rtree_of_tree left :: rtree_of_tree right :: [])
  | Br (Some(x), Lf, right) -> RBr (
  | Br () *)

(* 6.9 *)
type ('a, 'b) xml = XLf of 'b option | XBr of 'a * ('a, 'b) xml list;;

type token = PCDATA of string | Open of string | Close of string;;

let addressbook =
  XBr ("addressbook", [
        XBr ("person", [
                XBr ("name", [XLf (Some "Atsushi Igarashi")]);
                XBr ("tel", [XLf (Some "075-123-4567")])]);
        XBr ("person", [XLf None]);
        XBr ("person", [XLf None])]);;

let addressbook_token_list = [Open "addressbook"; Open "person"; Open "name"; PCDATA "Atsushi Igarashi"];;

(* let rec xml_of_tokens : (token list -> (string, string) xml) = function
    [] -> XLf None
  | x :: rest -> let rec tokens_rec (t: token) (data: token list) =
    match t with
        PCDATA data -> XLf (Some data)
      | Open _ -> tokens_rec t data
      | Close tag -> XBr (tag, [tokens_rec t data])
  in tokens_rec x rest;; *)

