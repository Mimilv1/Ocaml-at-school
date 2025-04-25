(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

let range (n : int ) : int list =
  let rec range_aux (k : int) ( acc : int list ) : int list =
    match k with
    |0 -> acc 
    |_ -> range_aux (k -1) (k:: acc)
  in
  range_aux n []
;;


let rec somme_liste (l:int list) :int = match l with
  |[]->0
  |a::q->a+somme_liste q;;





let rec mem (l:'a list) (n:'a) :bool = match l with
  |[]->false
  |a::q -> if a==n then true else mem q n;;

mem (range(10000000)) 10000000;;(*Non elle n'a pas le même problème*)
                                
                                
  
let rec range2 (n : int ) : int list =
  match n with 
  |0 -> []
  |_ -> n :: range2 (n -1)
;;

range2 (1000000);;
somme_liste (range(1000000));;

(*range aux est récursive terminale*)

let rec somme_liste2 (l:int list) :int = 
  let rec somme_aux (q : int list ) (acc : int) : int = 
    match q with
    |[]-> acc
    |a::d -> somme_aux d (acc+a)
  in
  somme_aux l 0;;
  
somme_liste2 (range(1000000));;
let c = (range(1000000))@(range(1000000));; (*@n'est pas récursif terminal*)
  
let rec ieme (l:'a list) (n:int) :'a= 
  match l with
  |[]->(-1 )
  |a::q when n==1 ->a
  |a::q -> ieme (q) (n-1) ;;



ieme (range(1000000)) 1000001;;(*EZ PZ*)


let rec concat_term (u:'a list) (v:'a list) :'a list =
  let rec concat_aux (i:int) (acc:'a list) :'a list=
    match i with
    |
    
  in
  concat_aux u v;;



  
  
  
