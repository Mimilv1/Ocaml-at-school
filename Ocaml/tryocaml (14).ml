type 'a rn =
  |V
  |N of 'a rn * 'a * 'a rn
  |R of 'a rn * 'a * 'a rn
;;

let cons (t : 'a rn) (l : 'a rn) (x : 'a) (r : 'a rn) : 'a rn =
	(* Reconstruit un arbre rouge noir ayant pour racine Noeud(l,x,d)
		La couleur associée à la racine est celle de la racine de t
  *)
  match t with
  |V -> failwith "Impossible de copier la couleur"
  |N(_,_,_) -> N(l,x,r)
  |R(_,_,_) -> R(l,x,r)
;;

let corrige_rouge (arbre:'a rn) :'a rn = match arbre with 
  |N(R(R(t1,a,t2),b,t3),c,t4) -> N(R(t1,a,t2),b,R(t3,c,t4))
  |N(R(t1,a,R(t2,b,t3)),c,t4) -> N(R(t1,a,t2),b,R(t3,c,t4))
  |N(t1,a,R(R(t2,b,t3),c,t4)) -> N(R(t1,a,t2),b,R(t3,c,t4))
  |N(t1,a,R(t2,b,R(t3,c,t4))) -> N(R(t1,a,t2),b,R(t3,c,t4))
  |_-> arbre
;;

let rec insere_aux (arbre:'a rn) (value:'a) :'a rn = match arbre with
  |V->R(V,value,V)
  |N(g,x,d) when x=value -> N(g,x,d)
  |R(g,x,d) when x=value -> R(g,x,d)
  |N(g,x,d) -> if value>x then corrige_rouge(N(g,x,insere_aux d value)) else corrige_rouge(N(insere_aux g value,x,d))
  |R(g,x,d) -> if value>x then corrige_rouge(R(g,x,insere_aux d value)) else corrige_rouge(R(insere_aux g value,x,d))
;;

let insere (arbre:'a rn) (value:'a) :'a rn =
  let presque = insere_aux arbre value in
  match presque with
  |R(g,x,d) -> N(g,x,d)
  |_-> presque;;

let rec supprime_min (arbre:'a rn)  :'a rn * bool=
  match arbre with
  | V -> failwith "vide"
  | R (V, x, d) -> d,false
  | N (V, x, d) -> d,true
  | R (g, x, d) | N (g, x, d) ->
      let g1, a_diminue = supprime_min g in
      g1, a_diminue 
;;


let rec repare_noir_gauche (arbre:'a rn) (b:bool) :'a rn * bool = 
  if not b then arbre,b;
  match arbre with
  |




