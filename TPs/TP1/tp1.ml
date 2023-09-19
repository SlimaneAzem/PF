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

*)
let rec sigma f n = 
            match n with 
            | 0 -> f(0)
            | _ -> (f(n) + sigma(f n-1)) ;;
            
let carre x = x*x;;

sigma(carre 0);;
sigma(carre 3);;
sigma(carre 4);;
