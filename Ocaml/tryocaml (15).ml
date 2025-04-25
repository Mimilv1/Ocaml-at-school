type point = {x : float; y : float} ;;

let p1 = {x=0.;y=0.};;
let p2 = {x=5.;y=5.};;

let tab =[|p1;p2;{x=3.;y=3.};{x=100.;y=100.}|];;

let distance (p1:point) (p2:point) :float = ((p1.x -. p2.x)**2. +. (p1.y-.p2.y)**2.)**0.5;;

let dmin_naif (t:point array) : float = 
  let valeur = ref infinity in
  for i=0 to Array.length t -1 do
    for j = 0 to Array.length t -1 do 
      if distance t.(i) t.(j) < !valeur && i<>j then valeur := distance t.(i) t.(j)
    done;
  done;
  !valeur;;

distance p1 p2;;

dmin_naif tab;;

(*Il faut renvoyer zero on renvoit infinity, un on renvoie infinity, deux on renvoie la distance entre ces deux points*) 

let rec longueur (l:'a list) :int = match l with
  |[]->0
  |a::q-> 1 + longueur q;;

let separe_moitie (l:'a list) :'a list * 'a list = 
  let rec separe_moitie_aux (l:'a list) (n:int):'a list =
    if n>=1 then
      match l with
      |[]->[]
      |a::q -> a:: separe_moitie_aux q (n-1)
    else [] in
  let rec separe_moitie_aux2 (l:'a list) (n:int) (c:int) :'a list = 
    match l with
    |[]->[]
    |a::q -> if c >= n then a :: separe_moitie_aux2 q n c else separe_moitie_aux2 q n (c+1) in
  (separe_moitie_aux l ((longueur l)/2),separe_moitie_aux2 l ((longueur l)/2) 0);; 

separe_moitie [p1;p2;{x=3.;y=3.};{x=100.;y=100.};{x=100.;y=101.}];;
          

let compare_x (a:point) (b:point) :int =match (a,b) with
  |c,d when d.x>c.x -> -1
  |c,d when d.x=c.x -> 0
  |_,_ -> 1;;
  

let tri_par_x (l:point list) :point list = List.sort compare_x l;;
  
tri_par_x  [p1;p2;{x=3.;y=3.};{x=100.;y=100.};{x=100.;y=101.}];;
    
let mini (n1:float) (n2:float) :float = if n1>n2 then n2 else n1;;

let rec dmin_gauche_droite_aux (p1:point) (l1:point list) :float = match l1 with
  |[]->infinity
  |a::q -> mini (distance (p1) (a)) (dmin_gauche_droite_aux (p1) (q));;
                 
let rec dmin_gauche_droite (l1:point list) (l2:point list) :float = match l1 with
  |[] -> infinity
  |a::q -> mini (dmin_gauche_droite_aux a l2) (dmin_gauche_droite q l2);;
  
let rec dmin_dc_naif_aux (l:point list) : float = match l with
  |[] -> infinity
  |[a] -> infinity
  |[a;b]-> distance a b
  |a::b::q -> let l1,l2 = separe_moitie l in min(min(dmin_dc_naif_aux l1) (dmin_dc_naif_aux l2)) (dmin_gauche_droite l1 l2);; 
  
let dmin_dc_naif (l:point list) :float = dmin_dc_naif_aux (tri_par_x l);;

dmin_dc_naif [p1;p2;{x=3.;y=3.};{x=100.;y=100.};{x=100.;y=101.}];;

dmin_dc_naif [{x=0.;y=0.};{x=0.;y=0.};{x=1.;y=1.}];;

(* relation de recurrence T(n) = | T(0)=T(1)=T(2) = O(1)
                                {  T(n+1)=T(n)+O(n)
                                 |
Complexite en O(n^2) on a pas avancé

*)
(*Q1 car si les points ne sont pas dans l'encadrement on a que d inferieur entre les points a gauche et à droite
  
    
    
    
    
    
      