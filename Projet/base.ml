let puzzle1 = ([0;1;2;3;4;5;6;7;8], 3) ;;
let grille1 = fst puzzle1;;
let puzzle2 = ([1;4;2;3;0;5;6;7;8], 3);;
let grille2 = fst puzzle2;;

(* Position d'un entier dans une liste *)
let rec position grille x = 
  match grille with
    | [] -> failwith "position : Erreur"
    | e::r -> if (e = x) then 0
                        else (1+(position r x));;

(* Valeur d'une position dans une liste *)
let rec valeur grille p = 
  match (grille, p) with
  | ([], _) -> failwith "valeur : erreur"
  | (x::r, n) -> if n = 0 then x 
                          else (valeur r (p-1));;

(* Modifie la valeur d'une position dans une liste *)
let rec modifier_valeur_indice grille p v = 
  match (grille, p) with
  | ([], _) -> failwith "modifier_valeur_indice : erreur"
  | (x::r, n) -> if n = 0 then v::r 
                          else x::(modifier_valeur_indice r (p-1) v);;

(* echange les valeur dans deux lilstes *)
let echange grille v1 v2 = 
  let position_v1 = (position grille v1) and position_v2 = position grille v2
  in let traitement_v1 = modifier_valeur_indice grille position_v1 v2
      in modifier_valeur_indice traitement_v1 position_v2 v1;;

(* On declare un type direction *)
type direction = Droite | Bas | Gauche | Haut;;

