(* 



let calculTVA prixHT = prixHT *. (1. +. 20. /. 100.) ;;
calculTVA 100.0;;



let bisextile annee = (annee mod 400 = 0)  || (annee mod 4 = 0) && (annee mod 100 <> 0);;
bisextile 2023;;
bisextile 2016;;



let isLower caractere = int_of_char caractere >= 97 && int_of_char caractere <= 123;;
isLower 'z';;
isLower 'A';;



let moyenne x y = (x +. y) /. 2. ;;
moyenne 10. 12. ;;



let divisionEntiere x y = (x / y, x mod y);;
divisionEntiere 12 6;;
divisionEntiere 5 2;;



let puissance_4 x = let carre y = y*y
                    in carre (carre x) ;;
puissance_4 5;;



let lowerToUpper caracter = let isLower x = int_of_char x >= 97 && int_of_char x <= 123
                            in if isLower caracter then char_of_int(int_of_char caracter - 32)
                                                    else caracter
                            ;;
lowerToUpper 'c';;
lowerToUpper 'C';;


let rec fib n = 
            match n with 
            | 0 -> 0
            | 1 -> 1
            | _ -> (fib(n-1) + fib(n-2));;
                        
fib 0;;
fib 1;;
fib 4;;
fib 9;;


let rec sommePremiersCarre n =  
            match n with 
            | 0 -> 0
            | 1 -> 1
            | _ -> (n*n + sommePremiersCarre(n-1));;
            
sommePremiersCarre 2;;
sommePremiersCarre 3;;
sommePremiersCarre 4;;


let carre x = x*x;;
let id x = x;;
let rec sigma f n = 
            match n with 
            | 0 -> f(0)
            | _ -> (f(n) + sigma f  n-1 );;    
sigma id 0;;
sigma id 1;;



let rond f g = fun x -> g(f x);;
let inc x = x+1;;
rond inc inc 10;;
rond inc inc 13;;

*)

let abs x = 
  if x < 0. then (x *. -1.)
  else x;;

let rec newton x y eps = 
  if abs(y*.y -. x)<eps then y
  else newton x ((y+.x/.y)/.2.) eps;;

let racine x = 
  let eps=0.00001
  in let arret y = abs(y*.y -. x)<=eps
          and suivant y = ((y+.x/.y)/.2.)
      in let rec newton y = 
              if arret y 
                then y
                else newton(suivant(y))
            in newton x;; 
