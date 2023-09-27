
let liste1=[1; 2; 3; 4; 5];;
let liste2=[6; 7; 8; 9; 10];;

let rec longueur l =
    match l with
		| [] -> 0
		| _::r -> (1 + longueur r) ;;

let concat l1 l2 = l1@l2;;

let rec nieme n l =
	match (n, l) with
		| (_, []) -> failwith "liste vide"
		| (0, x::_) -> x
		| (_, _::r) -> (nieme (n-1) r) ;;
