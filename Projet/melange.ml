#use "base.ml";;

let rec melange puzzle iterations = 
  let (grille, tailleGrille) = puzzle
    in if iterations=0
      then grille
      else let random_direction = match (Random.int 4) with
                                      | 0 -> Haut
                                      | 1 -> Bas
                                      | 2 -> Gauche
                                      | 3 -> Droite
                                      | _ -> Haut 
            in if (deplacement_possible (position grille 0) tailleGrille random_direction) 
              then let newGrille = (deplacer puzzle random_direction)
                    in melange (newGrille, tailleGrille) (iterations-1)
              else melange puzzle iterations;;
                                  
let testerChemin puzzle chemin = 
  let (grille, tailleGrille) = puzzle
  in let puzzlResult = List.fold_right (fun m p -> ((deplacer p m), tailleGrille)) chemin puzzle
      in let (result, x) = puzzlResult
          in result;; 