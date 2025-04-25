open Graphe


(* affiche une clique *)
let print_clique (c : partie) : unit =
	for i = 0 to (Array.length c)-1 do
		if c.(i) then
			begin
			print_int i ;
			print_string " "
			end
	done;
	print_newline()

(* renvoie le cardinal d'une clique *)
let cardinal_clique (c : partie) =
	let res = ref 0 in
	for i = 0 to (Array.length c) - 1 do
		if c.(i) then res := !res + 1
	done;
	!res
	
(* on suppose que g est non orienté *)
let rec arete_existe (g : graphe) (i : int) (j : int) : bool =
	List.mem j g.(i)

let g1 = [| [2;3;4] ; [2;3;4] ; [0;1;3] ; [0;1;2] ; [0;1] |]
let g2 = [| [1;4;2] ; [0;2] ; [1;3;0] ; [2;4] ; [0;5;3] ; [4] |]


(*Dans g2 le cycle maxiamle est 5  1-2-3-4-0
	 Oui se G1,4 et g2,4 sont des instance positives*)

let est_clique (g:graphe) (p:partie) :bool = 
	let flag = ref true in
	let sommet = premier_indice p in
	for i=0 to taille-1 do
		if p.(i) then 
			for j=i+1  to taille-1 do
				if p.(j) then if not (List.mem j g.(i)) then flag := false;
			done;
	done;
	!falg
(*est_clique est en O(n²)*)
(*Clique dans P donc dans NP*)	 
(*Certificat : les partires de [|1,k|] , cardinalclique de est_clique est en temps polynomiale*)
