#use "base.ml";;

(*
Melange la grille iterations-fois   
*)
let rec melange puzzle iterations = 
  let (grille, tailleGrille) = puzzle
    (* Sorte d'une boucle for pour le nombre d'iterations *)
    in if iterations=0
      then grille
      else let random_direction = match (Random.int 4) with (* On choisit une direction au hasard *)
                                      | 0 -> Haut
                                      | 1 -> Bas
                                      | 2 -> Gauche
                                      | 3 -> Droite
                                      | _ -> Haut 
            (* 
            On check si le deplacement choisi est possible 
            Si possible alors on deplace selon la direction et appel recursif
            Sinon alors appel recursif sans decrementer iterations
            *)
            in if (deplacement_possible (position grille 0) tailleGrille random_direction) 
              then let newGrille = (deplacer puzzle random_direction)
                    in melange (newGrille, tailleGrille) (iterations-1)
              else melange puzzle iterations;;
                  
(* 
Suit un chemin sur une grille et renvoit la grille resultat 
En appliquand direction par direction dans la liste chemin
*)
let testerChemin puzzle chemin = 
  let (grille, tailleGrille) = puzzle
  in let puzzlResult = List.fold_right (fun m p -> ((deplacer p m), tailleGrille)) chemin puzzle
      in let (result, x) = puzzlResult
          in result;; 