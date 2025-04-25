(* type représentant un graphe *)
type graphe = (int list) array


(*____________Réduction de HAMILTONIEN à CNF-SAT__________*)
let varh (n:int) (s:int) (i:int) :int = n*s + i 

let clause_sommet_atteint (n:int) (s:int) (file:out_channel): unit = 
	for i=0 to n-1 do
		Printf.fprintf file "%d " (varh (n) (s) (i));
	done;
	Printf.fprintf file "\n";
	()

let fnc_sommets_atteints (n:int) (file:out_channel) : unit = 
	for s= 0 to n-1 do
		clause_sommet_atteint n s file;
	done;
	()

let clause_sommet_une_fois (n:int) (s:int) (i:int) (j:int) (file:out_channel) :unit =
	Printf.fprintf file "-%d -%d" (varh n s i) (varh n s j)

let fnc_sommets_une_fois (n:int) (file:out_channel) :unit = 
	for s=0 to n-1 do
		for i = 0 to n-1 do
			for j=i+1 to n-1 do
				clause_sommet_une_fois n s i j file;
			done;
		done;
	done;

let clause_quelque_part (n:int) (i:int) (file:out_channel) =
	for s=0 to n-1 do
		Printf.fprintf file "%d " varh n s i
	done;
	Printf.printf file "\n"

let fnc_quelque_part (n:int) (file:outchannel) :unit = 
	for i = 0 to n-1 do
		clause_quelque_part n i file;
	done;

let clause_non_ubiquite (n:int) (s:int) (t:int) (i:int) file :unit = 
	Printf.fprintf file "-%d -%d" (varh n s i) (varh n t i)

let fnc_non_ubiquite (n:int) (file:out_channel) :unit = 
	for s = 0 to n-1 do
		for t=s+1 to n-1 do 
			for i= 0 to n-1 do
			if s<>t then clause_non_ubiquite s t i
			done;
		done;
	done;

let clause_deplacement (g:graphe) (s:int) (i:int) (file: out_channel) :unit =
	let n = Array.length g in
	Printf.fprintf file "-%d " (varh n s i);
	let rec aux_voisins = 
		match voisins with 
		|[]-> Printf.fprintf "\n"
		|t::s->Printf.fprintf file "%d " (varh n t (i+1)); aux v
	in aux g.(s)

let fnc_deplacement (g:graphe) (file:out_channel) = 
	let n =Array.length g in 
	for i=0 to n-2 do
		for s=0 to n-1 do
			clause_deplacement g s i file;
		done
	done;

let fnc_hamiltonien (g:graphe) (u:int) (v:int) (file:out_channel)
(*__________MAIN__________*)

let _ =
	let g2 = [|
		[1] ;
		[3] ;
		[1] ;
		[0;4] ;
		[2]
	|] in
	let g3 = [|
		[1] ;
		[2] ;
		[3] ;
		[1]
	|] in
	let file = open_out "test1.txt" in
	clause_sommet_une_fois 5 5 1 3 file;
	()
