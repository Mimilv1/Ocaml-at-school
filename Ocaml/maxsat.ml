type clause = int list
type cnf = clause list
type valuation = bool array
(* convention : les clauses contiennent des entiers non nuls *)
(* l'entier i représente la variable "x_i" si i > 0*)
(* l'entier -i représente le littéral "non x_i" si i > 0*)
(* dans une valuation, la valeur de x_i est donnée par la case i-1 *)


(* donne la valeur de vérité du littéral i *)
let valeur_litteral (v : valuation) (i : int) : bool =
	if(i > 0) then
		v.(i-1)
	else
		not v.((-i)-1)

(* affiche la valuation partielle donnant les valeurs de x_1 ... x_i inclus *)
let affiche (v : valuation) (i : int) : unit =
	for j = 1 to i do
		print_int j ;
		print_string " = " ;
		print_string (if valeur_litteral v j then "V" else "F") ;
		print_string " ; "
	done


let rec max_list (l:int list) :int = match l with
	|[]->0
	|a::q -> max (abs(a)) (max_list (q))

let rec max_var_cnf (f:cnf) = match f with
	|[]->0
	|a::q-> max (max_list (a)) (max_var_cnf q) 

let peut_sat_clause (c:clause) (v:valuation) (i :int) :bool= match c with
	|[] -> false
	|x::t -> (abs(x))>i || valeur_litteral v x || peut_sat_clause t v i

let peut_sat_cnf (f:cnf) (v:valuation) (i:int) :bool = 
	match f with
	|[]->true
	|a::q -> peut_sat_clause a v i && peut_sat_cnf q v i

let nb_clauses_satisfiable (f:cnf) (v:valuation) (i:iation -> int -> int telle si v est une valuation
partielle définie sur les variables de 1 à i, alors eval f v i renvoie le nombre de clant) :int = 
	match f with
	|[]->0
	|a::q -> if peut_sat_clause a v i then 1 + nb_clauses_satisfiable q v i
	else nb_clauses_satisfiable q v i

let gain (f:cnf) (v:valuation) :int= nb_clauses_satisfiable f v (Array.length v) (*Array.length v = max_var_cnf f*)

let eval (f:cnf) (v:valuation) (i:int) :int = 
	nb_clauses_satisfiable f v i

let copie_dans (a:'a array) (b:'a array) :()= 
	for i=0 to Array.length a -1 do
		b.(i)<-a.(i)
	done;

let maxsat_par_profondeur (f:cnf) :valuation = 
	let s_max = Array.make false max_var_cnf (f)

let f1 = [[1;2;-3];[1;-2;3];[-1;2;3]]
let f2 = [[-1;2];[-2;-1];[1;-2];[2;1]]
let f3 = [[1;2;3];[1;-3;-4];[1;-4];[-1;2;3];[-1;-2];[-1;-3];[2;-3];[-2;3]]
let f4 = [[1;2;3];[1;-3;4];[1;-4];[-1;2;3];[-1;-2];[-1;-3];[2;-3];[-2;3]]
