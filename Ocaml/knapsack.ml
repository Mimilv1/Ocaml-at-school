type sac = bool array

(* affiche les objets d'indice 0, ..., i-1, inclus présents dans le sac *)
let affiche (s : sac) (i : int) : unit =
	for j = 0 to i-1 do
		print_int j ;
		print_string " = " ;
		if s.(j) then
			print_string " Y ; "
		else
			print_string " N ; "
	done


let p1 = [|1;2;1;3;2|]
let v1 = [|8;10;2;5;3|]
let limite1 = 6

