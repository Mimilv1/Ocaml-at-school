type 'a arbre =
  | Feuille of 'a
  | Noeud of 'a arbre * 'a * 'a arbre
;;


let mon_arbre = 
  Noeud(
    Noeud(
      Feuille 1,
      2,
      Noeud(
        Feuille 5,
        6,
        Feuille 7
      )
    ),
    8,
    Feuille 12
  )
;;

let arbre_exemple =
  Noeud (
    Feuille " Blanc ",
    " Gris ",
    Noeud (
      Noeud (
        Feuille " Cyan ",
        " Violet ",
        Feuille " Magenta "
      ) ,
      " Noir ",
      Feuille " Jaune "
    )
  )
;;

let rec peigne_gauche (n:int) :unit arbre = match n with
  |0-> Feuille ()
  |a-> Noeud(peigne_gauche (n-1),(),Feuille ());;

let rec hauteur_arbre (tree:'a arbre) :int = match tree with
  |Feuille a -> 0
  |Noeud (a,b,c) -> max (hauteur_arbre (a) +1) (hauteur_arbre (c) +1);;

let rec taille (tree: 'a arbre) :int = match tree with
  |Feuille a ->1
  |Noeud (a,b,c) -> taille a + taille c +1;; 

let rec parfait (n:int) :unit arbre = match n with
  |0-> Feuille ()
  |a -> let sous_a = parfait(n-1) in Noeud(sous_a,(),sous_a);;


type operator =
  | Minus
  | Plus
  | Times
  | Div
  | Pow (* Uniquement des nombres positifs dans l’ exposant *)
;;

type expr_arith =
  | Entier of int (* Une constante entiere *)
  | Calcul of expr_arith * operator * expr_arith (* Un calcul dont l’ expression principale est
operator , et les op´e randes sont deux expressions *)
;;

let calcul:expr_arith =
  Calcul
    (
      Calcul
        (
          Entier 2,
          Times,
          Calcul
            (
              Entier 5,
              Pow,
              Calcul
                (
                  Calcul
                    (
                      Entier 4,
                      Plus ,
                      Entier 2
                    )
                ,Minus,
                Entier 3)
            )
        )
    ,Div,
    Entier 18
    )
;;

let rec eval_op (op:operator) (a:int) (b:int) :int = match op with
  |Plus -> a + b
  |Minus -> a-b
  |Times -> a*b
  |Div->a/b
  |Pow -> if b=0 then 1 else a*eval_op Pow a (b-1);;


let rec eval_arith (exp:expr_arith) :int = match exp with
  |Entier a ->a
  |Calcul (a,b,c) -> eval_op b (eval_arith a) (eval_arith c);;


let rec mem_arbre (a:int) (tree:int arbre) :bool = match tree with
  |Feuille b -> a==b
  |Noeud (h,j,k) -> a==j || mem_arbre a h ||mem_arbre a k;;

let rec min_arbre tree:int arbre = match tree with
  |Feuille a -> a
  |Noeud (a,b,c) -> min(b) (min(min_arbre a) (min_arbre c)) ;;

let rec max_arbre tree:int arbre = match tree with
  |Feuille a -> a
  |Noeud (a,b,c) -> max(b) (max(max_arbre a) (max_arbre c)) ;;


let rec est_ABR tree:int arbre :bool=match tree with
  |















