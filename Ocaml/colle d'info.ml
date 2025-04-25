type 'a arbre_binaire =
  | Fin
  | Noeud of 'a arbre_binaire * 'a * 'a arbre_binaire
;;

let rec prof (arb: 'a arbre_binaire): int =
  match arb with
  | Fin -> 0
  | Noeud (g, _, d) -> 1 + max (prof g) (prof d)
;;

(* Questions préliminaires *)

let rec somme (arb: int arbre_binaire): int =
  match arb with
  | Fin -> 0
  | Noeud (g, e, d) -> e + (somme g) + (somme d)
;;

(* Recherche *)

let rec recherche (abr: 'a arbre_binaire) (v: 'a): bool =
  match abr with
  | Fin -> false
  | Noeud (g, e, d) ->
      if v <= e then v = e || recherche g v
      else recherche d v
;;

(* Quelle est sa complexité en fonction de la taille N de l’arbre
   (nombre de noeuds de l’arbre) ? Distinguer les cas en moyenne et au pire.
   A quel type d’arbre correspond la complexité la plus médiocre ? 

  La complexité est en O(N) dans le pire des cas (si l'arbre est un peigne et
  que l'on cherche le dernier noeud). Cependant en moyenne la complexité est en
  O(H) où H est la hauteur, c'est-à-dire en O(log(N)).
  Si l'arbre est complet, on divise à chaque itération le nombre de noeuds à 
  traiter par deux.
*) 

(* Insertion *)

let rec insertion (abr: 'a arbre_binaire) (v: 'a): 'a arbre_binaire =
  match abr with
  | Fin -> Noeud(Fin, v, Fin)
  | Noeud(g, e, d) ->
      if v <= e then Noeud((insertion g v), e, d)
      else Noeud(g, e, (insertion d v))
;;

(* Quelle est la complexité de cette opération ?
   
  La même que pour recherche, vu qu'on parcours l'arbre de la même façon.
*)

let rec construit (u: 'a list): 'a arbre_binaire =
  match u with
  | [] -> Fin
  | x::xs -> insertion (construit xs) x
;;

(* Parcours *)

let rec parcours (abr: 'a arbre_binaire): 'a list =
  match abr with
  | Fin -> []
  | Noeud(g, e, d) -> (parcours g)@[e]@(parcours d)
;;

(* Suppression *)

let rec suppression (abr: 'a arbre_binaire) (v: 'a): 'a arbre_binaire =
  match abr with
  | Fin -> Fin
  | Noeud(g, e, d) when v > e -> Noeud(g, e, suppression d v)
  | Noeud(g, e, d) when v < e -> Noeud(suppression g v, e, d)
  | Noeud(g, e, d) ->
      begin
        match (g, d) with
        | (Fin, Fin) -> Fin
        | (Fin, _) -> d
        | (_, Fin) -> g
        | (_, _) ->
            begin
              let rec max (abr: 'a arbre_binaire): 'a =
                match abr with
                | Fin -> failwith "arbre vide"
                | Noeud (_, e, Fin) -> e
                | Noeud (_, _, g) -> max g
              in
              let m = max(g) in
              Noeud(suppression g m, m,d)
            end
      end
;;

(* On doit forcément parcourir l'arbre jusqu'à la racine, donc la complexité est
   au moins propotionelle à N (la taille de l'arbre, si ce dernier est un peigne)
   Elle est même en O(n) comme on fait a chaque fois un seul appel récursif et
   max est aussi en O(n) et est appellé d'une au plus une seule fois.
*)
