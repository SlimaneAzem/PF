type arbrebin = Feuille of int | Noeud of (arbrebin*arbrebin);;

let arbreTest = Noeud (Noeud (Feuille 1, Feuille 2), Noeud (Noeud (Feuille 3, Feuille 4), Feuille 5));;
let arbreTest_copy = Noeud (Noeud (Feuille 2, Feuille 3), Noeud (Noeud (Feuille 4, Feuille 5), Feuille 6));;
let arbreTest2 = Noeud (Noeud (Noeud (Feuille 1, Feuille 2), Noeud (Feuille 3, Feuille 4)), Noeud (Noeud (Feuille 5, Feuille 6), Noeud (Feuille 7, Feuille 8)));;

let rec nombre_noeudsInternes a =
	match a with 
		Feuille _ -> 0
		| Noeud (sag, sad) -> (1+nombre_noeudsInternes(sag)+nombre_noeudsInternes(sad));;

let rec nombre_feuilles a = 
	match a with 
		Feuille _ -> 1
		| Noeud (sag, sad) -> (nombre_feuilles(sag)+nombre_feuilles(sad));;

let rec profondeur a = 
	match a with
		Feuille _ -> 0
		| Noeud (sag, sad) -> let profondeur_sag = profondeur sag and profondeur_sad = profondeur sad
													in if profondeur_sad>profondeur_sag then 1+profondeur_sad
																															else 1+profondeur_sag;;
let rec arbre_meme_type a b = 
	match (a, b) with 
		(Feuille _, Feuille _) -> true
		| Noeud (a1, a2), Noeud (b1, b2) -> ((arbre_meme_type a1 b1) && (arbre_meme_type a2 b2))
		| (_, _) -> false;;

let rec arbre_to_liste a =
	match a with 
		Feuille x -> [x]
		| Noeud (sag, sad) -> ((arbre_to_liste sag) @ (arbre_to_liste sad));;

(* arbre_to_liste arbreTest;;
arbre_to_liste arbreTest_copy;;
arbre_to_liste arbreTest2;; *)

let rec map_arbre f a =	
	match a with
		Feuille x -> (Feuille (f x))
		| Noeud (sag, sad) -> (Noeud ((map_arbre f sag), (map_arbre f sad)));;

arbre_to_liste arbreTest;;
arbre_to_liste (map_arbre (fun x -> x+1) arbreTest);;