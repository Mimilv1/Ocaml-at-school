type sommet = int;;
type graphe = sommet list array;;

let test_graphe = [|[1];[2];[0]|]
let g0 = [|[1;2];[2;3;4];[];[0;5];[1;2];[10];[1;9];[8];[6];[7;10];[11];[5]|]
let g1 = [|[1;4];[0;2;4;7];[1;5];[6;8];[0;1];[2;7;8];[3;8];[1;5;8];[3;5;6;7];[]|]


let dfs (pre :sommet->unit) (post: sommet->unit) (env:graphe) (a:sommet) :unit =
  let n = Array.length env in
  let vus = Array.make n false in
  let rec aux (v:int) =
    if not(vus.(v)) then
      begin
        vus.(v)<- true;
        pre v;
        List.iter aux env.(v);
        post v;
      end
  in
  aux a;;
  
let rien (v:int) :unit= let a = [|0|] in 
  a.(0)<-v
;;

dfs (print_int) (rien) (test_graphe) (0);;

(*
  let dfs (s1 :sommet->unit) (s2:sommet->unit) (env:graphe) (a:sommet) :unit =
    let n = Array.length env in
    let vus = Array.make n false in
    let a_voir = [a] in
    let parcours
  *)
  
let ouvre x = Printf.printf "Ouverture %d\n" x;;
let ferme x = Printf.printf "Fermeture %d\n" x;;



let bfs (pre:sommet->unit) (env:graphe) (a:sommet) :unit =
  let n = Array.length env in
  let vus = Array.make n false in
  let a_voir = Queue.create () in
  Queue.push a a_voir;
  while not(Queue.is_empty a_voir) do
    let y = Queue.pop a_voir in
    if not(vus.(y)) then
      begin
        vus.(y)<- true;
        pre y; 
        let rec sucesseur (l:int list) :unit = match l with
          |[]->()
          |a::q -> Queue.push a a_voir;
              sucesseur q  in 
        sucesseur env.(y); 
      end 
  done;
;;
  
bfs ouvre g0 0;;
bfs ouvre g1 5;;
    
(*pour faire des files on peut utiliser deux piles*)

let rec voisins (n:sommet) (env:graphe) :int list= match env.(n) with
  |[]->[]
  |a::q
;;

let fontiere (pre :sommet->unit) (post: sommet->unit) (env:graphe) (a:sommet) :unit=
  let n = Array.length env in
  let vus = Array.make n false in
  let frontiere = ref [] in 
  let nouveau = ref [] in
    
    