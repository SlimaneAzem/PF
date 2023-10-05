type operateur = Mult | Plus | Moins;;
type arbre = 
  C of int
  | N of (operateur * arbre list);;

let arbre_test = N(Plus, [C 1; N(Mult, [C 5; C 2; C 3]); C 2; N(Mult, [C 3; C 2])]);;
let arbre_test2 = N(Plus, [C 1; N(Mult, []); C 2; N(Mult, [C 3; C 2])]);;

let rec nombre_constantes e = 
  match e with 
    | C _ -> 1
    | N (_, li) -> (List.fold_left (fun x y -> (nombre_constantes y)+x) 0 li);;

let rec expression_est_correcte e = 
  match e with 
    | C _ -> true
    | N (_, []) -> false
    | N (_, li) -> (List.fold_left (fun x y -> (expression_est_correcte y)&&x) true li);;

let rec eval e = 
  if (expression_est_correcte e) then match e with 
                                      | C x -> x
                                      | N (Mult, li) -> (List.fold_left (fun x y -> (eval y)*x) 1 li)
                                      | N (Plus, li) -> (List.fold_left (fun x y -> (eval y)+x) 0 li)
                                      | N (Moins, li) -> (List.fold_left (fun x y -> (eval y)-x) 0 li)
                                else failwith "expression incorrecte";;

let rec chaine_de_arbre e = 
  if (expression_est_correcte e) then match e with 
                                      | C x -> (string_of_int x)
                                      | N (Mult, e::r) -> (List.fold_left (fun x y -> x^"*"^(chaine_de_arbre y)) ("("^chaine_de_arbre e) (r))^")"
                                      | N (Plus, e::r) -> (List.fold_left (fun x y -> x^"+"^(chaine_de_arbre y)) ("("^chaine_de_arbre e) (r))^")"
                                      | N (Moins, e::r) -> (List.fold_left (fun x y -> x^"-"^(chaine_de_arbre y)) ("("^chaine_de_arbre e) (r))^")"
                                      | _ -> failwith "expression incorrecte"
                                else failwith "expression incorrecte";;
chaine_de_arbre (C 3);;
chaine_de_arbre (N(Plus, [C 1; C 4; C 7]));;
chaine_de_arbre (N(Plus, [C 1; N(Mult,[C 5; C 2])]));;
chaine_de_arbre arbre_test;;