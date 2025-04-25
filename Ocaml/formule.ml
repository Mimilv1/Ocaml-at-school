(* type d'une formule logique *)
type formule =
	Var of int
	| Non of formule
	| Et of formule * formule
	| Ou of formule * formule
	| Implique of formule * formule

(* type d'une valutaion *)
type valuation = bool array

(* type d'une forme normale conjonctive *)
(* liste de clauses *)
type fnc = (int list) list

(* affiche une formule *)
let rec affiche_formule (f : formule) =
	match f with
	| Var i -> Printf.printf "%d" i
	| Non g -> Printf.printf "NON " ; affiche_formule g
	| Et(f1,f2) -> Printf.printf "(" ; affiche_formule f1 ; Printf.printf " /\\ " ; affiche_formule f2 ; Printf.printf")" 
	| Ou(f1,f2) -> Printf.printf "(" ; affiche_formule f1 ; Printf.printf " \\/ " ; affiche_formule f2 ; Printf.printf")" 
	| Implique(f1,f2) -> Printf.printf "(" ; affiche_formule f1 ; Printf.printf " -> " ; affiche_formule f2 ; Printf.printf")" 

(* affiche une forme normale conjonctive *)
let rec affiche_fnc (f : fnc) =
	List.iter (
		fun l -> List.iter (fun x -> Printf.printf "%d " x) l ; Printf.printf "\n"
	) f

(* a^b *)
let rec puissance (a : int) (b : int) =
	assert (not (a = 0 && b = 0));
	if a = 0 then 0
	else if b = 0 then 1
	else 
		begin
		let x = puissance a (b/2) in	
		if b mod 2 = 0 then x*x
		else a*x*x
		end

(* renvoie le nombre de variables d'une formule *)
let rec nb_var (f : formule) : int =
	match f with
	| Var i -> i
	| Non g -> nb_var g
	| Et(f1,f2) | Ou(f1,f2) | Implique(f1,f2) -> max (nb_var f1) (nb_var f2)

(* renvoie le nombre de variables d'une formule *)
let rec nb_var_fnc (f : fnc) : int =

	let rec nb_var_clause (c : int list) : int =
		match c with
		| [] -> 0
		| x::t -> max (abs x) (nb_var_clause t)
	in
	match f with
	| [] -> 0
	| c::t -> max (nb_var_clause c) (nb_var_fnc t)

let rec satisfait (f:formule) (v:valuation) :bool = match f with
	| Var i -> v.(i)
	| Non g -> not (satisfait (g) (v))
	| Et(f1,f2) -> (satisfait (f1) (v)) && (satisfait (f2) (v))
	| Ou(f1,f2) -> (satisfait (f1) (v)) || (satisfait (f2) (v))
	| Implique(f1,f2) -> (not (satisfait (f1) (v))) || (satisfait (f2) (v)) 

let prochaine_valuation (v:valuation) :unit = 
	let n = Array.length v in
	let stop = ref false in
	for i = n-1 downto 1 do
		if (not !stop) then(
			 (if (not (v.(i))) then stop:= true);
			 v.(i)<- (not (v.(i)));)
	done;
	();;

let table (f:formule) :unit =
	let n = nb_var f in
	let v = Array.make (n+1) (false) in
	for i=1 to n do 
		Printf.printf "x%d " i;
	done;
	Printf.printf "f";
	for i=1 to puissance (2) (n) do
		Printf.printf "\n";
		for j=1 to n do
			if v.(j) then Printf.printf "V  " else Printf.printf "F  ";
		done;
		if (satisfait (f) (v)) then Printf.printf "V  " else Printf.printf "F  ";
		prochaine_valuation (v);
	done;
	Printf.printf "\n";;

table(Et(Var 1, Var 2));;

let rec check (a:int list) (v: valuation) :bool = match a with
	|[]->false
	|a::q -> if a>0 then v.(a) || check (q) (v)
	else (not v.(a)) || check (q) (v);;

let rec satisfait_fnc (fn:fnc) (v:valuation) :bool = match fn with
	|[] -> true
	|a::q -> check (a) (v) && satisfait_fnc (q) (v);;

let rec elimination_implique (f:formule) : formule =
	match f with
	|Var i -> Var i
	|Non i -> Non (elimination_implique (i))
	|Et (f1,f2) -> Et(elimination_implique f1, elimination_implique f2)
	|Ou (f1,f2) -> Ou(elimination_implique f1, elimination_implique f2)
	|Implique (f1,f2) -> Ou(Non(elimination_implique (f1)), elimination_implique(f2));;

let rec descente_non (f:formule) : formule = match f with
	|Non(Et(i,j))-> Ou(descente_non(Non((i))),descente_non(Non(j)))
	|Non(Ou(i,j)) -> Et(descente_non(Non(i)),descente_non(Non(j)))
	|Et(i,j) -> Et(descente_non(i), descente_non(j))
	|Ou(i,j) -> Ou(descente_non(i), descente_non(j))
	|Non(Non(i))-> descente_non(i)
	|Implique(i,j) -> failwith "BIG BOOM BADA BOOM"
	|_-> f;;

(*Question 7 admis*)

let fnc_simple (f:formule) :fnc = 