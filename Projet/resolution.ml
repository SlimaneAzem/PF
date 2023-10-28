#use "melange.ml";;

(* 
  Retourne la liste resultant de tout les deplacements possibles a partir de la grille [Haut; Gauche; Droite; Bas]
  retourne une liste de couples [(grille , chemin a partir de la grille initiale)]
*)
let liste_grilles_chemins (grilleInit, chemin) taille = 
  (* On declare la callback qui rajoute la grille resultat dans la listeGrilles*)
  let callback listeGrilles direction = 
    (*
    On check si le mouvement est possible
    Si Possible alors on calcule la grille resultat et on l'ajoute a la liste (on ajoute aussi la direction au chemin)
    Sinon alors on retourne la grilleListes sans modifs
    *)
    if (deplacement_possible (position grilleInit 0) taille direction) 
      then let newGrille = (deplacer (grilleInit, taille) direction)
            in listeGrilles@[(newGrille, direction::chemin)]
      else listeGrilles
  in List.fold_left (callback) [] [Haut; Gauche; Droite; Bas];;

(*
Parcours Naif
  RESULTATS TESTS : 
    resoudre [3;1;2;4;7;5;0;6;8] [0;1;2;3;4;5;6;7;8] 3 1 -> 250ms 
    resoudre [6;3;1;4;0;2;7;8;5] [0;1;2;3;4;5;6;7;8] 3 1 -> 3min
    resoudre [1;4;3;6;2;8;7;5;0] [0;1;2;3;4;5;6;7;8] 3 1 -> Stack overflow apres 17min
*)
let rec parcours1 grille_finale taille prochaines_grilles = 
  match prochaines_grilles with
  | [] -> failwith "parcours1: erreur"
  (* Si la prochaine grille est la bonne alors on retourne son chemin *)
  | (grille, chemin)::r -> if grille=grille_finale
                            then chemin
                              (* Sinon on rajoute toutes les grilles attengnables en un mouvement 
                              a partir de la premiere a la fin de prochaines_grilles et on rappelle *)
                            else (parcours1 grille_finale
                                            taille 
                                            (r@(liste_grilles_chemins (grille, chemin) taille)));;

(*
Parcours Naif (version programmation dynamique) 
  RESULTATS TESTS : 
    resoudre [3;1;2;4;7;5;0;6;8] [0;1;2;3;4;5;6;7;8] 3 2 -> 228ms
    resoudre [6;3;1;4;0;2;7;8;5] [0;1;2;3;4;5;6;7;8] 3 2 -> 256ms
    resoudre [1;4;3;6;2;8;7;5;0] [0;1;2;3;4;5;6;7;8] 3 2 -> 6.7s
*)
let rec parcours2 grille_finale taille prochaines_grilles grilles_parcourues = 
  match prochaines_grilles with
  | [] -> failwith "parcours2: erreur"
  (* Si la prochaine grille est la bonne alors on retourne son chemin *)
  | (grille, chemin)::r -> if grille=grille_finale
                            then chemin
                              (* Sinon oncheck d'abord si on l'a deja parcourue 
                                  .Si parcourue alors on reappelle sur le reste des prochaines_grilles
                                  .Sinon on rajoute la grille aux grilles parcourues et on rajoute les
                                     grilles possibles aux prochaines_grilles
                              *)
                            else if (List.mem grille grilles_parcourues)
                                  then (parcours2 grille_finale taille r grilles_parcourues )
                                else (parcours2 grille_finale 
                                      taille 
                                      (r@(liste_grilles_chemins (grille, chemin) taille)) 
                                      (grille::grilles_parcourues));;
                                  
(* Calcule la distance entre deux grilles (considere les cases de la grille comme des points "geometriques" dans un repere)*)
let distance grille_initiale grille_finale taille = 

  (* Fonction valeur absolue*)
  let abs x = if x<0 then (-x) else x
  
  (* On declare notre fonction pour calculer la distance entre deux points *)
  in let distance_points pos_a pos_b = 
          (* Coordonnées de a et de b*)
          let (x, y) = (pos_a/taille, (pos_a mod taille)) and 
              (xp, yp) = (pos_b/taille, (pos_b mod taille))
            (* Distance entre les deux points *)
            in ((abs (xp-x))+(abs (yp-y)))

      (* On Declare notre callback pour le fold *)
      in let callback somme nombre =
              let position_initiale = (position grille_initiale nombre) and 
                  position_finale = (position grille_finale nombre)
              in somme + (distance_points position_initiale position_finale)

          (* On l'utilise pour calculer la distance (somme des distances de chaque nombre) *)
          in List.fold_left (callback) 0 grille_initiale;;  

(*
  Retourne la liste resultant de tout les deplacements possibles a partir de la grille [Haut; Gauche; Droite; Bas]
  retourne une liste de triplés [(grille , chemin a partir de la grille initiale, distance de la grille finale)]
*)
let liste_grilles_proches (grilleInit, chemin) grille_finale taille = 

  (* On declare la callback qui rajoute la grille resultat dans la listeGrilles*)
  let callback listeGrilles direction = 
    (*
    On check si le mouvement est possible
    Si Possible alors on calcule la grille resultat et on l'ajoute a la liste (on ajoute aussi la direction au chemin)
    Sinon alors on retourne la grilleListes sans modifs
    *)
    if (deplacement_possible (position grilleInit 0) taille direction) 
      then let newGrille = (deplacer (grilleInit, taille) direction)
            in listeGrilles@[(newGrille, direction::chemin, (distance newGrille grille_finale taille))]
      else listeGrilles
  (* On recupere toutes les grilles a un mouvement de la grille actuelle *)
  in List.fold_left (callback) [] [Haut; Gauche; Droite; Bas];;

(* 
  Trie une (int list * direction list * int) list selon le int (Trie une liste de grille*chemin*distance selon la grille)  
*)
  let rec sort l =
    match l with
      [] -> []
    | x :: r -> insert x (sort r)
  and insert elt l =
    match l with
      [] -> [elt]
    | (grille, chemin, distance) :: r -> let (g, c, d)=elt in  if d <= distance then elt :: l else (grille, chemin, distance) :: insert elt r;;
    
(* 
  Verifie si une grillea ete parcourue  (presentes dans une list de (grille, chemin, distance))
   (vu que dans parcours3 grille_parcourues=(int list*direction list*int) List.mem n'est pas utilisable)
*)
let rec grille_parcourue grille liste_grilles = 
  match liste_grilles with 
  | [] -> false
  | (x, _,_)::r -> if grille=x then true else (grille_parcourue grille r);;

(*
Parcours "Intelligent" (version programmation dynamique + tri par distance) 
  RESULTATS TESTS : 
    resoudre [3;1;2;4;7;5;0;6;8] [0;1;2;3;4;5;6;7;8] 3 3  -> 210ms
    resoudre [6;3;1;4;0;2;7;8;5] [0;1;2;3;4;5;6;7;8] 3 3  -> 220ms
    resoudre [1;4;3;6;2;8;7;5;0] [0;1;2;3;4;5;6;7;8] 3 3 -> 250ms
    resoudre [10;12;4;5;9;14;8;3;0;11;6;15;13;7;2;1] [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15] 4 3 -> 4min46s
*) 
let rec parcours3 grille_finale taille prochaines_grilles grilles_parcourues = 
  match prochaines_grilles with
  | [] -> failwith "parcours3: erreur"
  (* Si la prochaine grille est la bonne alors on retourne son chemin *)
  | (grille, chemin, distance)::r -> if grille=grille_finale
                                      then chemin
                                        (* Sinon oncheck d'abord si on l'a deja parcourue 
                                            .Si parcourue alors on reappelle sur le reste des prochaines_grilles 
                                                (qu'on trie par distance)
                                            .Sinon on rajoute la grille aux grilles parcourues 
                                                et on cherche les prochaines grilles a parcourir 
                                                (les grilles resultantes a partir de l'actuelle qu'on trie par distance)
                                        *)
                                      else if (grille_parcourue grille grilles_parcourues)
                                            then (parcours3 grille_finale taille (sort r) grilles_parcourues )
                                            else (parcours3 grille_finale 
                                                  taille 
                                                  (sort (r@(liste_grilles_proches (grille, chemin) grille_finale taille)))
                                                  ((grille, chemin, distance)::grilles_parcourues));;

(*
calcule le chemin entre finale et init (en choisissant le parcours
          1 -> Naif
          2 -> Dynamique (memorisassion)
          3 -> Disatances
)   
*)
let resoudre grille_init grille_finale taille parcours = 
match parcours with
  | 1 -> (parcours1 grille_finale taille [(grille_init, [])])
  | 2 -> (parcours2 grille_finale taille [(grille_init, [])] [])
  | 3 -> (parcours3 grille_finale taille [(grille_init, [], (distance grille_init grille_finale taille))] [])
  | _ -> [];;

let grille_finale = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15];;
let grille_init = [10;12;4;5;9;14;8;3;0;11;6;15;13;7;2;1];;

let chemin = resoudre grille_init grille_finale 4 3 ;;

(grille_finale) = (testerChemin (grille_init, 4) chemin);;