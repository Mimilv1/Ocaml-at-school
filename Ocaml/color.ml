(* type représentant un graphe *)
type graphe = (int list) array



(*____________Réduction de k-COLOR à CNF-SAT__________*)
let varc (k:int) (s:int) (i:int) :int = 1 + s*k + i

(*Clause chaque sommet doit posseder au moins une fonction *)

let clause_une_couleur (k:int) (s:int) (file:out_channel) :unit = 
	for i=0 to k-1 do
		Printf.fprintf file "%d " (varc k s i)
	done;
	Printf.fprintf file "\n"

let fnc_une_couleur (k:int) (n:int) (file:out_channel) :unit =
	for i=0 to n-1 do 
		clause_une_couleur (k) (i) (file);
	done;
	()

let clause_exlusion_couleur (k:int) (s:int) (i:int) (j:int) (file:out_channel) : unit = 
	Printf.fprintf file "-%d -%d\n" (varc k s i) (varc k s j)

let fnc_couleur_unique (k:int) (n:int) (file:out_channel) :unit = 
	for s=0 to n-1 do
		for i =0 to k-2 do
			for j = i+1 to k-1 do 
				clause_exlusion_couleur k s i j file
			done;
		done;
	done;
	()

let clause_arete (k:int) (s:int) (t:int) (i:int) (file:out_channel) :unit =
	Printf.fprintf file "-%d -%d\n" (varc k s i) (varc k t i)

let fnc_arete (k:int) (g:graphe) (file:out_channel) :unit =
	let rec aux s voisins i =
		match voisins with
		|[] -> ()
		|t::v -> if t>s then clause_arete k s t i file;
				aux s v i;
	in
	for i=0 to k-1 do
		for s=0 to (Array.length g) -1 do
			aux s g.(s) i
		done;
	done;
	()

let fnc_coloriable (k:int) (g:graphe) (file:out_channel) :unit =
	fnc_une_couleur k (Array.length g) file;
	fnc_couleur_unique k (Array.length g) file;
	fnc_arete k g file

(*__________MAIN__________*)

let _ =
	let g0 = [|
		[4;5;6] ;
		[2;5] ;
		[1;3;6] ;
		[2;4] ;
		[0;3] ;
		[0;1] ;
		[0;2]
	|] in
	let g1 = [|
		[2;5] ;
		[2;4;5] ;
		[0;1] ;
		[4;5] ;
		[1;3;6] ;
		[0;1;6] ;
		[4;5]
	|] in
	let file = open_out "test.txt" in
	fnc_coloriable 3 g0 file;
	()
