let l = [1; 2; 3; 4; 4; 5];;
let l2 = [5; 6; 7; 8];;
let ll = [l; l2];;

let longueur l = 
    (List.fold_left (fun x y -> x+1) 0 l, List.fold_right (fun x y -> y+1) l 0);;
longueur l;;

let concat l l2 = 
    l@l2;;
concat l l2;;

let met_a_plat ll = 
    (List.fold_left (fun x y -> x@y) [] ll, List.fold_right (fun x y -> x@y) ll []);;

met_a_plat ll;;

let supprime2 e l =
    (List.fold_left (fun x y -> if y!=e then x@[y] else x) [] l, List.fold_right (fun x y -> if x!=e then x::y else y) l []);;

supprime2 4 l;;

(*
let doublon l =
    List.fold_right (fun x y -> if List.mem x y then y else x::y) l [];;*)

let map f l =
    List.fold_right (fun x y -> (f x)::y) l [];;
map (fun x -> x+1) l;;