#use "melange.ml";;

let liste_grilles_chemins (grilleInit, chemin) tailleGrille = 
  let callback listeGrilles direction = 
    if (deplacement_possible (position grilleInit 0) tailleGrille direction) 
      then let newGrille = (deplacer (grilleInit, tailleGrille) direction)
            in listeGrilles@[(newGrille, direction::chemin)]
      else listeGrilles
  in List.fold_left (callback) [] [Haut; Bas; Gauche; Droite];;
