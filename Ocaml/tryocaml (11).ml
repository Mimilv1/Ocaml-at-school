type 'a tas = {tab : 'a array; mutable taille : int; compare : 'a -> 'a -> int};;

let tas_vide (x : 'a) (compare : 'a -> 'a -> int) : 'a tas =
	(* On a besoin d'un élément de type 'a pour créer le tas *)
  let tab = Array.make 1024 x in
  {tab =  tab; taille = 0; compare = compare}
;;

let propriete_locale (t : 'a tas) (i : int) =
  let rep = ref true in
  if 2*i+1 < t.taille && compare t.tab.(i) t.tab.(2*i+1) < 0 then
    rep := false
  ;
  if 2*i+2 < t.taille && compare t.tab.(i) t.tab.(2*i+2) < 0 then
    rep := false
  ;
  !rep
;;

let swap (t : 'a tas) (i : int) (j : int) : unit =
  let temp = t.tab.(i) in
  t.tab.(i) <- t.tab.(j);
  t.tab.(j) <- temp
;;

let rec tamiser (t : 'a tas) (i : int) : unit =
  match i with
  |0 -> ()
  |_ ->
      let parent = (i-1)/2 in
      if not (propriete_locale t parent) then
        begin
          swap t i parent;
          tamiser t parent
        end
;;

let ajouter (t : 'a tas) (x : 'a) : unit =
  assert(t.taille<1024);
  t.tab.(t.taille) <- x;
  t.taille <- t.taille + 1;
  tamiser t (t.taille -1)
;;

let rec tasser (t : 'a tas) (i : int) : unit =
  if not (propriete_locale t i) then
    begin
      let enfant_max = ref (2*i+1) in
      if (2*i+2 < t.taille) && (compare t.tab.(2*i+1) t.tab.(2*i+2) < 0) then
        enfant_max := 2*i+2
      ;
      swap t i (!enfant_max);
      tasser t (!enfant_max)
    end
;;

let rec retirer_racine (t : 'a tas) : 'a =
  assert(t.taille>0);
  let x = t.tab.(0) in
  swap t 0 (t.taille -1);
  t.taille <- t.taille -1;
  tasser t 0;
  x
;;


(* Test de la structure*)


let print_int_tas (t : int tas) =
  print_string "[";
  for i=0 to t.taille -1 do
    Printf.printf "%d, " t.tab.(i)
  done;
  print_string "]\n"
;;

let _ = 
  let t = tas_vide 5 (fun x y -> x-y) in
  ajouter t 10;
  ajouter t 25;
  ajouter t 3;
  ajouter t 7;
  print_int_tas t;
  assert(retirer_racine t = 25);
  print_int_tas t
;;

type ('p,'v) priority_queue = ('p * 'v) tas ;;

(* Une file de priorite est represeter par un tas creer en ajoutant les elements de la file un part un dans le tas*)

let compare_priority ((priorite1,valeur1):int*'v) ((priorite2,valeur2):int*'v) :int = priorite1-priorite2;;
  
let insert (p:'p) (v:'v) (t:('p,'v)priority_queue) :unit = ajouter t (p,v);;

let extract (t:('p,'v)priority_queue) :('p*'v) = retirer_racine t;;

let peek (t:('p,'v)priority_queue) :('p*'v) = if Array.length t.tab >0 then t.tab.(0) else failwith "AAAAAAAAA";;
  
(*1 et 2 Si on change le priorité d'un noeud alors il peut etre plus petit que avant et donc il est possible que 
ses enfants soit plus grands et donc le noeud peut violer la propriete de tas. Si il est plus grand que avant
il est possibles que sont père soit plus petit que lui et violerai donc la propritete de tas*)
  
  
  
  
  