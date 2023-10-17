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

(* Check si un deplacement est possible *)
let deplacement_possible position tailleGrille direction = 
  match direction with
  | Haut -> (position >= tailleGrille)
  | Bas -> (position < tailleGrille*(tailleGrille-1))
  | Gauche -> (position mod tailleGrille <> 0)
  | Droite -> (position mod tailleGrille <> (tailleGrille-1));;

(* Donne la nouvelle position du 0 selon une direction (on suppose le deplacement possible) *)
let nouvelle_position tailleGrille oldPos direction = 
  match direction with
  | Haut -> (oldPos - tailleGrille)
  | Bas -> (oldPos + tailleGrille)
  | Gauche -> (oldPos - 1)
  | Droite -> (oldPos + 1);;

let deplacer puzzle direction = 
  let (grille, tailleGrille) = puzzle
  in let position_0 = (position grille 0)
      in if (deplacement_possible position_0 tailleGrille direction) then let valeur_nouvelle_case = (valeur grille (nouvelle_position tailleGrille position_0 direction))
                                                                          in (echange grille 0 valeur_nouvelle_case)
                                                                    else grille;;
      
let test_unaire_deplacer resultat test = 
  if resultat=test then "good Shit"
                    else "No good shit";;

(* test_unaire_deplacer [1; 2; 3; 4; 5; 0; 6; 7; 8] (deplacer ([1;2;3;4;0;5;6;7;8],3) Droite);;
test_unaire_deplacer [1; 2; 3; 4; 7; 5; 6; 0; 8] (deplacer ([1;2;3;4;0;5;6;7;8],3) Bas);;
test_unaire_deplacer [1; 2; 3; 0; 4; 5; 6; 7; 8] (deplacer ([1;2;3;4;0;5;6;7;8],3) Gauche);;
test_unaire_deplacer [1; 0; 3; 4; 2; 5; 6; 7; 8] (deplacer ([1;2;3;4;0;5;6;7;8],3) Haut);;

test_unaire_deplacer [0; 1; 2; 3; 4; 5; 6; 7; 8] (deplacer ([0;1;2;3;4;5;6;7;8],3) Gauche);; *)
