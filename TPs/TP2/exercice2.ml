
let liste1=[1; 2; 3; 4; 5];;
let liste2=[6; 7; 8; 9; 10];;

let ll=liste1::liste2::[];;

let rec npremiers n l = 
	match (n, l) with 
		| (0, _) -> []
		| (_, []) -> failwith "liste trp petite/vide"
		| (_, x::r) -> (x::npremiers (n-1) r);;

let rec met_a_plat ll =
	match ll with
		| [] -> []
		| l::r -> l@(met_a_plat r);;

let rec paire_vers_liste l1 l2 = 
	match (l1, l2) with 
		| ([], []) -> []
		| ([], _) | (_, []) -> failwith "Listes de longuers differentes"
		| (e1::r1, e2::r2) -> ((e1, e2)::paire_vers_liste r1 r2);;


let rec liste_vers_paire l = 
	match l with
	| [] -> ([], [])
	| (x, y)::r -> ((x::Stdlib.fst (liste_vers_paire r)), (y::Stdlib.snd (liste_vers_paire r)));;

let rec supprime1 x l =
	match l with 
	| [] -> []
	| e::r -> if e=x then r
					else (e::supprime1 x r);;

let rec supprime2 x l =
	match l with 
	| [] -> []
	| e::r -> if e=x then supprime2 x r
					else (e::supprime2 x r);;	
					
let rec min_liste l = 
	match l with
	| [] -> failwith "Liste vide"
	| x::[] -> x
	| x::r -> let min_r=min_liste r
			in if x<min_r then x
				else min_r;;

let rec doublon l = 
	match l with
	| [] -> []
	| x::r -> x::(supprime2 x (doublon r));;
