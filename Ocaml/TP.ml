open Graphics
open Unix

let _ = Random.self_init ()

let construire_grille (n:int) (m:int) (p:float) :int array array  = 
  let ma = Array.make_matrix (n) (m) (0) in 
  for i=0 to n-1 do
    for j=0 to m-1 do
      let r = Random.float (1.) in
      let fn = float_of_int n in
      let fi = float_of_int i in
      let fj = float_of_int j in
      let fm = float_of_int m in
      if r<(p*.(1.-. 4.*.((fi-.((fn-.1.)/.2.))*.(fi-.(fn-.1.)/.2.)+.(fj-. (fm-.1.)/.2.)*.(fj-. (fm-.1.)/.2.))/.((fn-.1.)*.(fn-.1.) +. (fm-.1.)*.(fm-.1.)))) then
        ma.(i).(j)<- 1;
    done;
  done;
  ma

let hauteur = 960
let largeur = 960


let tracer_grille (g : int array array) (k:int) :unit = 
  let n = Array.length g in
  let m = Array.length g.(0) in
  Graphics.open_graph ("");
  Graphics.resize_window (largeur) (hauteur);
  for i=0 to n-1 do
    for j=0 to m-1 do
      if g.(i).(j) = 0 then Graphics.set_color (Graphics.white) else Graphics.set_color (Graphics.black);
      Graphics.fill_rect ((i)*k) (j*k) (k) (k);
    done;
  done;
  Unix.sleepf (10.);;

let existe_chemin (g: int array array) (i:int) (j:int) (k:int) (l:int) :bool =
  let n = Array.length g in
  let m = Array.length g.(0) in
  let explorer = Array.make_matrix (n) (m) (false) in
  let rec parcourt_cases (i:int) (j:int) (k:int) (l:int):bool = 
    if (explorer.(i).(j) =false) && (g.(i).(j)=0) then (
      if i=k && j=l then true else (
        explorer.(i).(j) <- true;
        let nord = if j<m-1 then parcourt_cases (i) (j+1) (k) (l) else false in
        let sud = if j>0 then parcourt_cases (i) (j-1) (k) (l) else false in
        let est = if i<n-1 then parcourt_cases (i+1) (j) (k) (l) else false in
        let ouest = if i>0 then parcourt_cases (i) (j) (k) (l) else false in
        nord || sud || est ||ouest;
      )
    )
    else false in
  parcourt_cases (i) (j) (k) (l);;

let g = construire_grille (50) (50) (0.5) in begin
tracer_grille (g) (18);
if existe_chemin (g) (0) (0) (49) (49) then print_endline "oui" ;
end;;


type tas = {tab:((int*int)*float) array; mutable taille:int} (*((x,y),prio)::q   tête de priorité maximale*)

let fils_gauche (t:tas) (i:int) : (int*int)*float = t.tab.(2*i)
let fils_droit (t:tas) (i:int) : (int*int)*float = t.tab.(2*i + 1)

let echanger (tab:'a array) (i:int) (j:int) :unit =
  let c = tab.(i) in
  tab.(i)<-tab.(j);
  tab.(j)<-c;;

let rec tasser (t:tas) (i:int) :unit = if i<>0 then begin (*O(log2(n))*)
  let parent = i/2 in
  let _,prio_p =  t.tab.(parent) in
  let _,prio_f = t.tab.(i) in
  if prio_f<prio_p then begin 
    echanger (t.tab) (parent) (i);
    tasser (t) (parent);
    end;
  end;;

let rec ajouter (i:int*int) (prio:float) (t:tas) :unit = (*O(log2(n))*)
  t.tab.(t.taille)<- (i,prio);
  tasser (t) (t.taille);
  t.taille <- t.taille +1;;

let est_vide (t:tas) :bool = t.taille = 0;; (*O(1)*)

let rec tamiser (t:tas) (i:int) :unit = (*O(log2(n))*)
  if 2*i=t.taille-1 then begin
    let _,prio_f = fils_gauche t i in
    let _,prio_p = t.tab.(i) in
    if prio_f<prio_p then begin
      echanger (t.tab) (2*i) (i);
      end
    end
  else if 2*i >= t.taille then begin
    let _,prio_f = fils_gauche t i in
    let _,prio_p = t.tab.(i) in
    let _,prio_d = fils_droit t i in
    if prio_f>prio_d && prio_p>prio_d then begin
      echanger (t.tab) (i) (2*i+1);
      tamiser t (2*i+1);
    end
    else if prio_d>prio_f && prio_p>prio_f then begin
      echanger (t.tab) (i) (2*i);
      tamiser t (2*i);
    end;
  end;;

let changer_priorite (t:tas) (i:int) (nouvel_prio:float) :unit = (*O(log2(n))*)
  let x,ancienne_prio = t.tab.(i) in 
  t.tab.(i) <- (x,nouvel_prio);
  if nouvel_prio > ancienne_prio then tamiser t i
  else tasser t i;;
  
let recherche (a:int*int) (t:tas) :int = (*O(n)*)
  let indice = ref (-1) in 
  for i=0 to t.taille -1 do 
    let valeur,_ = t.tab.(i) in
    if valeur=a then indice :=i;
  done;
  !indice;;

let changer_priorite_coord (a:int*int) (t:tas) (nouvel_prio:float) :unit = (*O(n)*)
  changer_priorite (t) (recherche (a) (t)) nouvel_prio;;
(*
let voisin_libre :int*int list = 

let a_etoile (g:int array array) (i:int) (j:int) (k:int) (l:int) :int*int list = 
  let n = Array.length g in
  let m = Array.length g.(0) in
*)