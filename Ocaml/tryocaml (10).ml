(*
   (* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)
(*1*)
  let norme a b = ((a)**2.+.(b)**2.)**0.5;;
  norme 1. 1.;;
(*2*)
  let moyenne a b = (a +. b)/.2.;;
  moyenne 5. 1.;;
(*3*)
  let f a b = (a+b)/2;;
  f 10 5;;
(*a le type de cette fonction est int -> int -> int*)
(*b f 10 5 vaut 7*)

  let float_of_int a = (float)a;;

  let moyenne_entiers a b = (float_of_int(a)+.float_of_int(b))/.2.;;
  moyenne_entiers 1 2;;
(*1.2*)
  abs(-3);; (*La fonction abs prend un entier et renvoie sa valeur absolue *)
  abs_float(-3.2);; (*La fonction abs_float prend en entréer un float et renvoie sa valeur absolue*)
  let abs a = if a>=0 then a else -a;;
  abs(-3);;

(*1.3*)
  let rec suite a = if a>0 then 3*suite(a-1)+2 else 4;;
  suite 0;;
  suite 1;;
(*1.4*)
let rec factorielle n = if n > 0 then n*factorielle(n-1) else 1;;
factorielle 3 ;;
(*2 sa renvoie 1*)
(*1.5*)
let rec somme_carres k = if k>0 then k*k+somme_carres(k-1) else 0;;
somme_carres 3;;
(*1.6*)
let rec puissance x n = if n>0 then (x*.puissance x (n-1)) else 1.;;
puissance 2. 3;;
let rec puissance_bis x n = if n>=0 then puissance x n else 1./.puissance x (-n);;
puissance_bis 5. (-3);;
(*3*)

(*1.7*)

let rec somme_liste l = match(l) with
  |[]->0.
  |a::q->a+.somme_liste(q);;
somme_liste [1.;2.;3.];;

let rec longueur l = match(l) with
  |[]->0
  |a::q->1+longueur q;;

let rec moyenne_liste l = somme_liste l /. float_of_int(longueur l);;

moyenne_liste [1.;2.;3.] 
  
let rec croissant l = match(l) with
  |[] -> true
  |a::[] -> true
  |a::y::q -> (a <= y) && croissant (y::q);;

croissant([1;2;3]);; 

let rec concat u v = match u with
  |[] ->v
  |a::[] -> a:: concat [] v
  |a::r::b -> a:: concat (r::b) v;;

concat [1;2] [1;3];;
concat [1;2;3;3] [8;9;10]
(*1.10*)
let rec miroir a = match a with
  |[]->[] 
  |a::q ->   miroir q@[a];;
             
miroir [1;2;3];;
  
let rec uniques u = match u with
  |[]->[] 
  |a::[]->[]
  |a::b::[]-> if a<>b then [a;b]@uniques (b::[])  else []@uniques (b::[])
  |a::b::q -> if a<>b then [a]@uniques (b::q)  else []@uniques (b::q);;

uniques [1;1;1;2;2;3;4;5;6;8;8;9];;




(*Exercice 1 TP,1.5*)
let rec dernier_element (l:'a list) :'a = match l with
  |[]-> failwith "La liste vide n’a pas de dernier element "
  |a::[]-> a
  |a::q-> dernier_element q;;

dernier_element [1;2;3;4] ;;

(*Exercice 2*)
let rec un_sur_trois (l:'a list) :'a list = match l with
  |[]->[]
  |a::[] -> [a]
  |a::r::[] -> [a]@[r]
  |a::r::q::d -> [a]@([r]@ un_sur_trois (d));;

un_sur_trois [1;1;1;2;2;2;3;3];;

(*Exercice 3*)
let rec insertion (l:'a list) (x:'a) :'a list = match l with
  |[] -> [x]
  |a::[] -> if x<a then [x]@[a] else [a]@[x] 
  |a::r -> if x<a then [x]@[a]@r else [a]@insertion r x;;

insertion [1;3] 2;;
      
let rec tri (l:'a list) :'a list = match l with
  |[]-> []
  |a::[]-> [a]
  |a::q -> insertion (tri (q)) (a) ;;

tri [100;5;9;6;2;1;1;7;4;7;3];;
  
   
(*Exercice 4*)
let rec recherche_lineaire (secret:int) (borne_inf:int) (borne_sup:int) :int =
  if borne_sup>=secret && secret>=borne_inf then
    if secret>borne_inf then recherche_lineaire (secret) (borne_inf+1) (borne_sup)
    else borne_inf
  else failwith "Stop";;

recherche_lineaire 1000000001 0 2000000000;;
recherche_lineaire 50 0 49 ;;

let rec recherche_dicho (secret:int) (borne_inf:int) (borne_sup:int) :int =
  if borne_sup>=secret && secret>=borne_inf then
    let centre = ((borne_inf+borne_sup)/2) in
    if secret>centre then recherche_dicho (secret) (centre) (borne_sup+1) 
    else if secret<centre then recherche_dicho (secret) (borne_inf-1) (centre) (*La division entière peut poser problème d'où le +1 -1*)
    else
      centre
  else failwith "Stop";;

recherche_dicho 1000000001 0 2000000000;;
recherche_dicho 50 0 49 ;;

(*Pour ceux qui ont fini tp 1*)
(*Exercice 1.12*)
let rec indice (x:'a) (l:'a list) :int = match l with
  |[]->(-1)
  |a::q when a=x -> 0
  |a::q -> 1+indice x q;;

indice 5.3 [1.1;3.2;5.2;6.3];;
(*Exercice 1.13*) 

let rec sous_liste_EZ (le:'a list) (li:int list) :'a list =
  let rec extract (l:'a list) (x:int) = match(l) with
    |[]->[]
    |a::q when x = 0 -> [a]
    |a::q-> extract (q) (x-1) in
  match li with
  |[]->[]
  |a::q-> (extract (le) (a)@ sous_liste_auxi le q);;

sous_liste_EZ [1;2;3;5;6] [5]

(*Exerccie II.1*)

let rec mem (x:'a) (u:'a list) :bool = match u with
  |[] -> false
  |a::q when a = x -> true
  |a::q -> mem x q;;
mem 5 [1;2;2];;

let rec nth (u:'a list) (n:int) :'a = match u with
  |[]->failwith "oupsi"
  |a::q when n = 0 -> a
  |a::q-> nth (q) (n-1) ;;

let rec take (n:int) (u:'a list) :'a list= match u with
  |[]->[]
  |a::q when n<=1 -> [a]
  |a::q-> a::take (n-1) q;;

take 5 [1;2;3;4;5;6];;

let rec range (a:int) (b:int) :int list = match a with
  |d when a=b -> [b]
  |d when a<b -> a::range (a+1) (b)
  |d when a>b -> []
  |_ -> failwith "range";;

range 5 1;;

(*Exercice II.2*)
let rec concat u v = match u with
  |[] ->v
  |a::[] -> a:: concat [] v
  |a::r::b -> a:: concat (r::b) v;;

(*concat si u est de lognuer n il exute n+1 operation*)

let rec miroir_naif a = match a with
  |[]->[] 
  |a::q ->   miroir_naif q@[a];;
(*miroir naif est qudratique n²*)

let rec rev_append (l:'a list) (l2:'a list) :'a list = match l with 
  |[]->l2
  |a::q -> rev_append q ([a]@l2) ;;

rev_append [1;2;3;4] [5;6;7;8];;


let miroir (l:'a list) :'a list = rev_append l [];;

miroir [1;2;3;6;3;5];;
(*Exercice II.3*)
let rec applique (f:'a -> 'b) (l:'a list) :'b list = match l with
  |[]->[]
  |a::q -> [f a]@applique f q;;

let  listes_carres (l:int list) :int list = applique (fun x -> x*x) l;;

listes_carres [1;2;3]
  
(*Exercice II.4*)
let rec uniques u = match u with
  |[]->[] 
  |a::[]->[]
  |a::b::[]-> if a<>b then [a;b]@uniques (b::[])  else []@uniques (b::[])
  |a::b::q -> if a<>b then [a]@uniques (b::q)  else []@uniques (b::q);;

let rec sans_doublons_triee (u:'a list) :bool = if u = uniques u then true else false;;
  
let rec elimine_doublons_triee (u:'a list) :'a list = match u with
  |[]->[] 
  |a::[]->[]
  |a::b::[]-> if a=b then [a] else a::[b]
  |a::b::q-> if a=b then elimine_doublons_triee (a::q) else a::b::elimine_doublons_triee q;;

sans_doublons_triee [1;1;1;1;1;2;2];;
elimine_doublons_triee [1;1;1;1;1;2;2];; 

let rec sans_doublons (u:'a list) :bool= match u with
  |[]->true
  |a::q-> not (mem a q) && sans_doublons q;;

sans_doublons [1;2;1]

let rec elimine_doublons (u:'a list) :'a list = match u with
  |[]->[]
  |a::b -> if mem a b then elimine_doublons b else a::elimine_doublons b;;

elimine_doublons [1;2;3;3;2]
(*"Sans doublons trie est de n Maximum atteint quand il n'y a aps de doublon"
"sans doublon est de n² car mem est de n et peut l'appliqer a chaque n soit n fois n donc complexité quadratique"*)
(*Exercice II.5*)
let rec compresse (l:'a list) :('a*int )list = 
  let rec double (l:'a list) :int= match l with
    |[]->
    |a::b::c-> if a = b then 1 + double b::c else 1
          
      
  in
  match l with
  |[]->[]
  |a::b::[] ->
  |a::b::c when a=b->(a,2+)
  |a::b::c=(a,1);; 

compresse ["a"; "b"; "b"; "b"; "a"; "c"; "c"; "d"];;


  (*Exercice 1 TD 2.5*)
let mon_test x = x>5;;
let rec trouver_element (fon:'a->bool) (l1:'a list) :'a=match l1 with
  |[]->failwith "BIG BOUM BADA BOUM"
  |a::q when fon a -> a
  |a::q->trouver_element fon q;;


trouver_element mon_test [1;2;9];;



let mon_test x = x>5;;
let rec trouver_element (fon:'a->bool) (l1:'a list) :'a option=match l1 with
  |[]->None
  |a::q when fon a -> Some a
  |a::q->trouver_element fon q;;


trouver_element mon_test [1;2;9];;
(*Exercice 2*)
let rec lesteteux (l:('a list) list) :'a list = match l with
  |[]->[]
  |a::q-> begin match a with
      |[]->lesteteux q
      |b::s -> b::lesteteux q end;;

lesteteux [[1;2];[];[];[4];[5;6;6]];;

(*Ex 3*)

let rec max_l (u:'a list) (v:'a list) :'a list = match (u,v) with
  |[],[]->[]
  |a::q,[]
  |[],a::q->failwith "PAS LA MEME TAILLE"
  |a::q,z::s -> (max a z)::max_l q s;;

max_l [1;2;3] [2;1;0];;


let rec max_l2 (u:'a list) (v:'a list) :'a list = match (u,v) with
  |[],[]->[]
  |a::q,[]
  |[],a::q->a::q
  |a::q,z::s -> (max a z)::max_l2 q s;;

max_l2 [1;2;3] [2;1;0;0;0;2];;
    
(*Exe 4*)
let rec fusion (u:'a list) (v:'a list) :'a list = match (u,v) with
  |[],[]->[]
  |a::q,[]
  |[],a::q->a::q
  |a::q,z::s-> if max a z = a then z::fusion (a::q) s else a::fusion q (z::s);;

fusion [1;4;9;16;25] [1;9;27;64;125];;


type mon_nombre =
  |Entier of int
  |Flottant of float;;

let mon_plus (a:mon_nombre) (b:mon_nombre) :mon_nombre = match(a,b) with
  |Entier a, Entier b -> Entier (a+b)
  |Flottant a, Flottant b -> Flottant (a+.b)
  |Flottant a, Entier b -> Flottant (a+.float_of_int b)
  |Entier a, Flottant b -> Flottant (float_of_int a+.b)
  |_,_->failwith "NOONNNOONON";;

let mon_moins (a:mon_nombre) (b:mon_nombre) :mon_nombre = match(a,b) with
  |Entier a, Entier b -> Entier (a-b)
  |Flottant a, Flottant b -> Flottant (a-.b)
  |_,_->failwith "NOONNNOONON";;

let mon_fois (a:mon_nombre) (b:mon_nombre) :mon_nombre = match(a,b) with
  |Entier a, Entier b -> Entier (a*b)
  |Flottant a, Flottant b -> Flottant (a*.b)
  |_,_->failwith "NOONNNOONON";;

let mon_divise (a:mon_nombre) (b:mon_nombre) :mon_nombre = match(a,b) with
  |Entier a, Entier b -> Entier (a/b)
  |Flottant a, Flottant b -> Flottant (a/.b)
  |_,_->failwith "NOONNNOONON";;


mon_divise (Entier 6) (Entier 3);;


(*Exercice 6*)

type 'a arbre =
  |Feuille of 'a
  |Noeud of 'a arbre*'a arbre;;

(*"Le nom du type indique arbre et un arbre binaire est composé de Feuille et de Noeud donc ça fait bien penser à un arbre"*)
(*2*)
let rec nb_feuilles (arbres:'a arbre) :int= match arbres with
  |Feuille x -> 1;
  |Noeud (x,y) -> nb_feuilles x + nb_feuilles y;;

nb_feuilles ((Noeud(Noeud(Feuille 1, Feuille 2), Feuille 2)));;

(*3*)
let rec contient (arbres:'a arbre) (element:'a) :bool= match arbres with
  |Feuille x -> x=element
  |Noeud (x,y) -> (contient x element || contient y element);;

contient (Noeud(Noeud(Feuille 1, Feuille 2), Feuille 2)) 3;;


(*Exercice 7*)

let rec decoupage (l:'a list) :('a list* 'a list) = match l with
  |[]->[],[]
  |a::[] -> [a],[]
  |a::b::q ->let q1,q2 = decoupage q in a::q1,a::q2;;

let rec tri_fusion (l:'a list) :'a list = match l with
  |[] -> []
  |a::[]->l;
  |_ ->
      begin
        let (liste1,liste2)=decoupage l in
        fusion (tri_fusion(liste1)) (tri_fusion(liste2));
      end;;
    




decoupage [1;2;3;6];;

tri_fusion [2;3;6;9;5;1];;


*)
(*Cour ARRAY*)
(*
  let somme (t:int array) :int = 
    let valeur = ref 0 in
    for i=0 to (Array.length t -1) do
      valeur := !valeur + t.(i)
    done;
    !valeur;;

  somme [|1;2;3|];;

  let nouveau_tableau_carres (t:int array) :int array = 
    let result = Array.make (Array.length t) 0 in
    for i=0 to (Array.length t -1) do
      result.(i) <- (t.(i)*t.(i))
    done;
    result;;


  nouveau_tableau_carres [|1;2;3|];;

  let array_make_matrix (n:int) (m:int) (e:'a) :'a array array =
    let resultat = Array.make n [||] in
    for i = 0 to n-1 do 
      resultat.(i) <- Array.make m e 
    done;
    resultat;;

  array_make_matrix 10 10 '0';;

  let array_nn (n:int) :'a array array =
    let result = array_make_matrix n n 0 in
    for i=0 to n-1 do
      for j=1 to n do
        result.(i).(j-1) <- i mod j
      done;
    done;
    result ;;
  
  array_nn 10;;


  
(*TP 3*)
(*Exercice III.1*)
let extrema (t:int array) :int*int = 
  let min = ref t.(0) in
  let max = ref t.(0) in
  for i=1 to (Array.length t - 1) do
    if t.(i)>(!max) then max := t.(i) else if t.(i)<(!min) then min := t.(i)
  done;
  (!min,!max);;

extrema [| 4; 1; 7; 2; 10; 6|];;

let nb_occs (valeur:'a) (l:'a array) :int = 
  let nombre = ref 0 in
  for i=0 to ((Array.length l) -1) do
    if valeur = l.(i) then
      nombre := !nombre + 1
  done;
  !nombre;;

nb_occs 5 [|1; 2; 5; 4; 5; 12; 18|];;

let tab_occs (t:int array) : int array =
  let result = Array.make (Array.length t) 0 in
  for i = 0 to (Array.length t -1)do 
    result.(i) <- nb_occs i t
  done;
  result;;
        
tab_occs [|1; 2; 5; 4; 5; 12; 18|];;
(*"Ordre de grandeur quadratique"*)

let tab_occs_eff (t:int array) : int array =
  let result = Array.make (Array.length t) 0 in
  for i = 0 to (Array.length t -1)do 
    if t.(i)< Array.length t then
      result.(t.(i))<- result.(t.(i)) + 1
  done;
  result;;

tab_occs_eff [|1; 2; 5; 4; 5; 12; 18|];;

(*Exercice III.2*)

let sommes_cumules (t:int array) : int array =
  let rec somme (valeur:int) (l:int array) :int = 
    if valeur >= 0 then 
      t.(valeur) + (somme (valeur-1) t)
    else
      0
  in
  let result = Array.make (Array.length t) 0 in
  for i = 0 to (Array.length t -1)do 
    result.(i) <- somme i t
  done;
  result;;


sommes_cumules [|2; 1; 0; 7; 10|];;


(*Exercice III.3*)
let array_map (f:'a -> 'b) (t:'a array) :'b array =
  if Array.length t>0 then
    let result = Array.make (Array.length t) (f(t.(0))) in
    for i=0 to (Array.length t -1) do
      result.(i) <- f(t.(i))
    done;
    result
  else
    [||];;


array_map (fun i -> 2 * i - 1) [|2; 4; 1; 5|];;


let array_init (n:int) (f:int->'a) :'a array = 
  let result = Array.make n 0 in
  for i=0 to n-1 do
    result.(i)<- f(i)
  done;
  result;;

array_init 5 (fun i -> i * i);;

let array_list (t:'a array) :'a list = 
  let liste = ref [] in 
  for i=0 to Array.length t -1 do
    liste:= !(liste)@[t.(i)]
  done;
  !liste;;
  
  
array_list [|2; 5; 1|];;
  
  
let list_array (l:'a list) :'a array= 
  if l<>[] then
    let t = ref (Array.make (List.length l) (List.hd l))  in
    let l1 = ref l in
    for i=0 to (List.length l)-1 do
      !t.(i)<-List.hd (!l1);
      l1:=List.tl (!l1)
    done;
    !t
  else
    [||];;


list_array [2; 5; 1];;
  
(*Exercice III.4*)
let rec some (l:'a list) :int= match l with
  |[]->0
  |a::q ->a + some q;;
  
let rec produit (l:'a list) :int= match l with
  |[]->1
  |a::q ->a * produit q;;
  
produit [1;2;3];;
some [1;2;5];;
  
  
let rec applatit (l:'a list list) :'a list = match l with
  |[]->[]
  |a::q -> a@applatit q;;
  
applatit [[1;3;7];[1;103;7];[1;3;7]];;
  
  
let rec max_list (l:int list) :int = match l with
  |[] -> failwith "liste vide"
  |a::[] -> a
  |a::q::r -> if q>a then max_list(q::r) else max_list(a::r);;

max_list [1;3;2;1;1;1];;
  
let rec reduction (f:'a->'b->'b) (l:'a list) (b:'b) :'b = match l with
  |[]->b
  |a::q ->f (a) (reduction f q b);;
  

reduction (fun (x:int) (y:float) :float-> y +. float_of_int x) [1;2;3;2;2] 12.1;;

let some_reduc (l:'a list) :int = reduction (fun a b -> a+b) l 0;;

some_reduc [1;2;3;2;2];;
  
let produit_reduc (l:'a list) :int = reduction (fun a b -> a*b) l 1;;

produit_reduc [1;2;3;4]

let applatit_reduc (l: 'a list list) :'a list =  reduction (fun (a:'a list) (b:'a list) -> a@b) l [];;

applatit_reduc [[1;3;7];[1;103;7];[1;3;7]];;

let max_liste_reduc (l: 'a list) :'a= reduction max l min_int;;

(*Exercice III.5*)

   (*
   
(*"consecutif"*)
let rec consecutif_list (l:int list) :bool=
  match l with
  |[]->false
  |a::[]->false
  |a::q::r-> (a+1)=q || consecutif_list (q::r);;
  
consecutif_list [1 ;3 ;4 ;8];;

let consecutif_array (t:int array) :bool =
  let resultat = ref false in
  for i=0 to Array.length t -2 do
    if t.(i) + 1 = t.(i+1) then resultat:=true
  done;
  !resultat;;

consecutif_array [|1;4;9;5|];;

*)
(*TD 5*)

let f (n:int) :int = 
  let u = ref 1 in
  let v = ref 1 in
  let g = ref 1 in
  if n<2 then n*1
  else
    begin
      for i = 2 to n do
        u := !v;
        v := !g;
        g := !u + !v;
      done; 
      !g;
    end;;

for i=0 to 100 do
  print_int (f(i));
  print_string "\n"
done;;
  
let fibo_efficace (n : int ) : int =
  let rec aux (i : int ) (f1 : int) (f2 : int ) : int =
     (* Calcule F_n , sachant que f1 = F_i et f_2 = F_(i +1) 4 Pr´e condition : 0 <= i <= n *)
    if n>i && i>=0 then
      aux (i+1) (f2) (f1+f2)
    else
      f1
  in aux (0) (0) (1)
;;
for i=0 to 20 do
  print_int (fibo_efficace i);
  print_string "\n"
done;;

(*Exo7*)
  
let rec retirer (l:'a list) (x:'a) :'a list = match l with
  |[]->failwith "Error"
  |a::q -> if a=x then q else a::retirer q x;;

(*Complexité theta(n) donc linéaire
Dans le pire des cas x se trouve au bout de la liste donc on parcourt la longueur n de la liste
Ainsi c'est un theta(n)*)

let mini (l:'a list) :'a = 
  let rec mini_aux (l:'a list) (x:'a) :'a= match l with
    |[] -> x
    |a::q -> if a<x then mini_aux q a else mini_aux q x in
  mini_aux (l) (List.hd l);;
  

let rec tri_selection (l:'a list) :'a list = match l with
  |[]->[]
  |a::q-> let n = mini(l) in n::tri_selection  (retirer l n);;

let tri_notes (l:int list) : int list= 
  let t = Array.make 0 21 in 
  let rec tri (l: int list) :unit = match l with
    |[]-> ()
    |a::q -> begin
        t.(a) = t.(a) +1 ;
        tri q
      end in
  tri l;
  
                
;;

combi t 0;;
    (*O(n²)*)
    
(*5 Prenons une liste quelqu'onque de taille n  *)
(*
(*TD 6*)  
(*Exo 1*)
  
  type 'a my_stack = 'a list ;;

  let (pile_vide:'a my_stack) = [];;
  let is_empty (p:'a my_stack) :bool = p=[];;
  let depiler (p:'a my_stack) :('a*'a my_stack) =if is_empty p then failwith"DePiLaTiOn ErRoR" else(List.hd p, List.tl p);;
  let empiler (p:'a my_stack) (x:'a) :'a my_stack = x::p;;


  

(*Exo 2*)

  type 'a stack = 'a list ref;;

  let pile_vide_i :'a list ref = ref [];; 
  let depiler_i (p:'a stack) :'a = let value = List.hd !p in
    p:= List.tl !p;
    value;;
  let enpiler (p:'a stack) (x:'a) :unit= p:= x::!p;;
  let is_empty_i (p:'a stack) :bool = (!p)=[];;
  
  

(*Exo 3*)
(*"Capacite correspond au nombre d'élément dans la pile Sommet correspond a la valeur au sommet de la pile"*)
  type 'a my_stack_2 = { capacite : int ; mutable sommet : int ; contenu : 'a array };;

  let pile_1 = {capacite=1; sommet=0; contenu=[|42|]};;
  let pile_2 = {capacite=3; sommet=2; contenu=[|"un";"deux";"trois"|]};;
  let pile_v = {capacite =0; sommet=(-1);contenu=[||]};;
  
  
  let is_empty_i2 (p:'a my_stack_2) :bool=p.sommet=(-1);;
  is_empty_i2 pile_v;;

(*Une pile vide est définit par un sommet égale a -1*)
  let creer_pv (elem:'a) :'a my_stack_2= {capacite=0; sommet=(-1); contenu=(Array.make 0 elem)};;
  
  let push (p:'a my_stack_2) (x:'a) :unit = if p.sommet=p.capacite then failwith"pile pleine" else 
      begin 
        p.sommet<- p.sommet + 1;
        p.contenu.(p.sommet)<-x
      end;;

  let pop (p:'a my_stack_2) = if p.sommet <> (-1) then 
      begin
        let v = p.contenu.(p.sommet) in
        p.sommet<- p.sommet - 1;
        p.contenu.(p.sommet) <- ();
        v
      end
    else
      failwith"Pile vide";;
    
(*Exo 4*)

  type 'a my_stack_3 = {mutable capacite : int ; mutable sommet : int ; mutable contenu : 'a array};;

  let push_2 (p:'a my_stack_3) (x:'a) = 
    if p.capacite<>0 then
      begin
        if p.sommet=p.capacite then 
          begin p.capacite<-p.capacite+p.capacite ; 
            p.sommet<- p.sommet + 1;
            p.contenu<- Array.append (Array.append p.contenu [|x|]) (Array.make ((p.capacite/2)-1) x)
          end
        else
          begin 
            p.sommet<- p.sommet + 1;
            p.contenu.(p.sommet)<-x
          end
      end
    else failwith"Pile vide";;

(*La complexité dans le pire des cas est O(n)*)

  push_2 {capacite=2;sommet=1;contenu=[|0;2;1|]} 5;;

*)
*)

exception FileVide ;; (* erreur relative a une file vide *)
exception FilePleine ;; (* erreur relative a une file d´e passant la capacit ´e maximale *)


type 'a my_queue =
  {
    capacite : int;
    mutable arriere : int ;
    mutable avant : int ;
    mutable vide : bool ;
    contenu : 'a array
  };; (*capacite correspond à la capacite de la file arriere correspond l'indice de l'arrière de la file
      et avant correspond à l'indice de l'avant vide dit si c'ets une fiel vide contenu est le tableau
      qui contient les élements*)

let file_vide = {capacite=0; arriere=0; avant=0; vide=true; contenu=[||]};;

let file_q2 = {capacite=1; arriere=0; avant=0; vide=false; contenu=[|42|]};;

let file_q2 = {capacite=3; arriere=0; avant=2; vide=false; contenu=[|"trois";"deux";"un"|]};;


let ma_file =
  {
    capacite = 4;
    arriere = 2;
    avant = 0;
    vide = false ;
    contenu = [|0;42;5;8|]
  } ;; 

(*Cette file représente une file de 4 de capacite
avec les élement dans l'ordre 5 8 0 *)

let give_fv (capa: int) (tipeu:'a) :'a my_queue = {capacite=capa; arriere=1; avant=0; vide=true; contenu= Array.make capa tipeu};;

let is_empty (f:'a my_queue) :bool = f.vide;;

let defile (f:'a my_queue) : 'a*'a my_queue =
  if not (is_empty f)
  then 
    f.contenu.(f.avant), 
    if f.avant=f.arriere 
    then
      begin
        f.vide<-true;
        f
      end
    else
    if f.avant = 0 
    then
      begin
        f.avant <- f.capacite -1;
        f
      end
    else 
      begin
        f.avant<-f.avant -1;
        f
      end 
  else raise FileVide;;

let defile2 (file:'a my_queue) :'a = 
  if not (is_empty file) then
    begin
      file.vide <- file.avant = file.arriere; 
      file.avant <- (file.avant-1+file.capacite) mod file.capacite;
    end
  else
    raise FileVide ;;



let enfiler (f:'a my_queue) (x:'a) :unit = 
  if f.avant <> f.arriere -1 && not (f.arriere <> 0)
  then
    if f.arriere - 1 < -1
    then 
      begin
        f.vide <- false; 
        f.arriere <- f.arriere - 1;
        f.contenu.(f.arriere) <- x 
      end
    else
      begin
        f.vide <- false; 
        f.arriere <- f.capacite - 1;
        f.contenu.(f.arriere) <- x 
      end
  else raise FilePleine;;

enfiler {capacite = 4; arriere = 3; avant=1; vide =false; contenu=[|0;6;9;8|]} 3;;
  
(*Exercie 2*)
type 'a stack = 'a list ;; (*CELLE CI*)

let (pile_vide:'a stack) = [];;
let is_empty (p:'a stack) :bool = p=[];;
let depiler (p:'a stack) :('a*'a list)=if is_empty p then failwith"DePiLaTiOn ErRoR" else (List.hd p, List.tl p);;
let empiler (p:'a stack) (x:'a) :'a stack = x::p;;

type 'a my_queue = 
  {
    arriere : 'a stack ;
    avant : 'a stack
  }

let extraire (j:'a*'a list) :'a = let (c,d) = j in c;;  

let rec vider (p1:'a stack) (p2:'a stack) :'a stack = 
  match p1 with
  |[]->p2
  |a::q -> vider q (empiler (p2) (extraire (depiler (p1))));;

let rec vider2 (stack1:'a stack) (stack2:'a stack) :'a stack= match stack1 with
  |[] -> stack2
  |a::q -> vider2 q (a::stack2);;

let vider_3 (stack:'a stack) :'a stack= 
  vider2 stack [];;

vider([1;2;3;6;5;4]) ([2;3;6;8;4]);; (*Oui le haut de la pile se palce au debut de ceel ci*)

let give_fvf :'a my_queue ={arriere = []; avant=[]} ;;

let est_vide (f:'a my_queue) :bool= f.arriere == f.avant && f.arriere = [] ;;
  
let rec defile_f (f:'a my_queue) :'a = if est_vide f then failwith "Erreur 923" (* on peut également utiliser un try with*)
  else if(f.avant == []) then defile_f {arriere=[];avant=(vider(f.arriere) (f.avant))}
  else List.hd f.avant;;

let enfile (f:'a my_queue) (n:'a) :'a my_queue = {arriere = n::(f.arriere) ; avant = f.avant};;


(*Exercice 3*)
(*"Solution :
""Problèmes 1 
""take prend longtermps

""Complexité amortie.
""i.e Sur N opérations, le temps du calcul est O(N)

""Problème 2
""danq quel ordre fait on les opérations

""Banque :
""à chaque opérations facile on garde du temps en banque
""Au lieu de s'executer en 4 opérations élémentaires on dit qu'elle s'execute en 6 et on garde 2 en banque

""Lorqu'on fait une opération difficile, on utilise le temps en banque plutot que le vrai tesmp.

""On vuet que Dop(s) = O(1) pour tous les op et 5

""Potentiel
""P :Ensembles des structures --> N
""P(s0) = 0

()
""ici la file est vide

""Représente le temsp en banque
""          op
""Si    S -----> S'

""Dop(S) = Cop (S) + P(S') - P(S)
*)
(*1
    Vider a une complexité linéaire O(n)
      give_fvf
      est_vide 
      defile_f
      enfile sont en complexite theta(1)
    
        
Donc la coplexité dans le pire des vas de Vider est en O(n) on en déduit que la complexité amortie est en
O(1)
    
      
2)
  Si toutes les complexité sont en O(1) alors notons k le nombre d'opération avec une complexité
    en O(1) on a donc que l'on peut majorer par un O(k) or k est constant donc par  un grand O(1)
      
  
  
  3)a) P prend une strucutre en entréer et y associe un entier supérieur ou égale à 0 ansi c'est bien un potentiel
    b)
*)
""1000 Lignes