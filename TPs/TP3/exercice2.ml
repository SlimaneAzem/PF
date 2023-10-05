type operateur_bin = Mult | Add;;
type operateur_un = Moins;;
type arbre = 
  Const of int
  | Var of string
  | Noeud1 of (operateur_un * arbre)
  | Noeud2 of (operateur_bin * arbre * arbre);;

let arbre_test = Noeud2 (Add, Noeud2(Mult,Var "x", Const 3), Var "y");;

let traduire_operation_binaire op =
  match op with
  Mult -> "*"
  | Add -> "+";;

let traduire_operation_unaire op =
  match op with
    Moins -> "-";;

let eval_operation_binaire op operande1 operande2 =
  match op with
  Mult -> operande1*operande2
  | Add -> operande1+operande2;;

let eval_operation_unaire op operande1 =
  match op with
    Moins -> -operande1;;

let rec chaine_de_arbre e = 
  match e with
    Const x -> (string_of_int x)
    | Var x -> x
    | Noeud1 (op, a) -> ("("^(traduire_operation_unaire op)^(chaine_de_arbre a)^")")
    | Noeud2 (op, a, b) -> ("("^(chaine_de_arbre a)^(traduire_operation_binaire op)^(chaine_de_arbre b)^")");; 

let rec is_variable_liee var li = 
  match li with 
    [] -> false
    | (nom, valeur)::r -> if nom=var then true
                                      else (is_variable_liee var r) ;;
  
let rec close e li = 
  match e with
    Var x -> (is_variable_liee x li)
    | Const (_) -> true
    | Noeud1 (_, a) -> (close a li)
    | Noeud2 (_, a, b) -> (close a li && close b li);;

let rec value_of_var var li = 
  match li with 
    [] -> failwith "erreur variable non liée"
    | (nom, valeur)::r -> if nom=var then valeur
                                      else (value_of_var var r) ;;

let rec eval e li = 
  if close e li then match e with
                      Const x -> x
                      | Var x -> (value_of_var x li)
                      | Noeud1 (op, a) -> (eval_operation_unaire op (eval a li))
                      | Noeud2 (op, a, b) -> (eval_operation_binaire op (eval a li) (eval b li))
                else failwith "expression non close";;

eval arbre_test [("x", 10); ("y", 15)];;