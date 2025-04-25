(* Term : lettres terminales a,b,c, etc... ; NonTerm i : non terminal X_i *)
type symbole = Term of char | NonTerm of int

(* suite de terminaux et non-terminaux *)
type mot_partiel = symbole array

(* règles de dérivation *)
type regle = (symbole * mot_partiel)

(* symbole initial, et liste de couples symbole -> mot partiel *)
type grammaire = (regle array)

let g_arith =
	[|
	(NonTerm 0 , [| NonTerm 3 ; NonTerm 0 |] ) ;
	(NonTerm 0 , [| NonTerm 2 ; NonTerm 4 |] ) ;
	(NonTerm 0 , [| NonTerm 8 ; NonTerm 5 |] ) ;
	(NonTerm 0 , [| Term 'a' |] ) ;
	(NonTerm 0 , [| Term 'b' |] ) ;
	(NonTerm 1 , [| NonTerm 2 ; NonTerm 4 |] ) ;
	(NonTerm 1 , [| NonTerm 8 ; NonTerm 5 |] ) ;
	(NonTerm 1 , [| Term 'a' |] ) ;
	(NonTerm 1 , [| Term 'b' |] ) ;
	(NonTerm 2 , [| NonTerm 8 ; NonTerm 5 |] ) ;
	(NonTerm 2 , [| Term 'a' |] ) ;
	(NonTerm 2 , [| Term 'b' |] ) ;
	(NonTerm 3 , [| NonTerm 1 ; NonTerm 6 |] ) ;
	(NonTerm 4 , [| NonTerm 7 ; NonTerm 1 |] ) ;
	(NonTerm 5 , [| NonTerm 0 ; NonTerm 9 |] ) ;
	(NonTerm 6 , [| Term '+' |] ) ;
	(NonTerm 7 , [| Term '*' |] ) ;
	(NonTerm 8 , [| Term '(' |] ) ;
	(NonTerm 9 , [| Term ')' |] ) ;
	|]

let g_2 =
	[|
	(NonTerm 0 , [| NonTerm 0 ; NonTerm 1 |] ) ;
	(NonTerm 0 , [| NonTerm 1 |] ) ;
	(NonTerm 0 , [| NonTerm 2 |] ) ;
	(NonTerm 1 , [| Term 'a' |] ) ;
	(NonTerm 1 , [| NonTerm 3 ; NonTerm 3 |] ) ;
	(NonTerm 1 , [| NonTerm 1 ; NonTerm 3 |] ) ;
	(NonTerm 2 , [| Term 'b' |] ) ;
	(NonTerm 3 , [| Term 'c' |] ) ;
	(NonTerm 3 , [| |] )
	|]

let print_symbole (x : symbole) =
	match x with
	| Term c -> Printf.printf "%c" c
	| NonTerm i -> Printf.printf "X%d" i

let print_mot_partiel (m : mot_partiel) =
	Array.iter print_symbole m

let afficher_grammaire (g : grammaire) =
	Array.iter (fun (x,a) -> print_symbole x ; Printf.printf " -> " ; print_mot_partiel a ; Printf.printf "\n") g

let cyk (s:string) (g:grammaire) :bool = 
	(* But de l'algo : On calcul E0,0 ... En-1,n-1
		 puis les E0,1...
		 on remplie une matrice
		 puis on regarde si S est dans E0,n-1 si oui true sinon false*)
	let n = String.length s in 
	let mat = Array.make_matrix n n [] in
	for i = 0 to n-1 do
		for j =0 to n-1 do
			match g.(i) with
			|(x, [|Term d|]) when d=i -> mat.(i).(i) <- x::mat.(i).(i) 
		done;
	done;
