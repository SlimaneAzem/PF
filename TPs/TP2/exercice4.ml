(*
*)

let l = [1; 2; 3; 4];;
let ll = [[]; [4]; [3]; [3; 4]; [2]; [2; 4]; [2; 3]; [2; 3; 4]; [1]; [1; 4]; [1; 3]; [1; 3; 4]; [1; 2]; [1; 2; 4]; [1; 2; 3]; [1; 2; 3; 4]];;

let inserer_tete x ll = 
    List.map (function l -> x::l) ll;;

let rec parties l = 
	match l with 
		| [] -> []
		| x::[] -> [[]; [x]]
		| x::r -> let parties_reste = parties r
					in (parties_reste@(List.map (function l -> x::l) parties_reste));;
