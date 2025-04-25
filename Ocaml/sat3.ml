open Graphe

type clause3 = int array

type cnf3 = clause3 array

let cnf3_to_cnf (f : cnf3) : Dpll.cnf =
	List.init (Array.length f) (fun i -> Array.to_list (f.(i)))

let phi1 = [| [|1;2;-3|] ; [|-1;-2;-3|] ; [|1;-2;-3|] |]
let phi2 = [| [|1;2;-3|] ; [|1;-2;3|] ; [|-1;2;3|] |]
let phi3 = [| [|-1;2;2|] ; [|-2;-1;-1|] ; [|1;-2;-2|] ; [|2;1;1|] |]

let sommets_caluses_clique (j:int) (p:int) :int = p*j+j

let reduction_clause (c:cnf3) : (graphe * int) 