module PrioQ :
	sig
		type t		
		val new_queue : int -> t (* l'entier est la capacité *)
		val mem : t -> int -> bool
		val extract_min : t -> (int * float)
		val insert : t -> (int * float) -> unit
		val decrease_priority : t -> (int*float) -> unit
		val is_empty : t -> bool
	end = struct
		type t =
			{mutable last : int;
			priorities : float array;
			keys : int array;
			mapping : int array}
		
		let is_empty q =
			q.last = -1
			
		let new_queue capacity =
			let priorities = Array.make capacity 0. in
			let keys = Array.make capacity (-1) in
			let mapping = Array.make capacity (-1) in
			let q = {
				last = -1;
				priorities = priorities;
				keys = keys;
				mapping = mapping;
			} 
			in q
			
		let mem q x = q.mapping.(x) <> -1
			
		let swap tab i j = 
			let temp = tab.(i) in
			tab.(i) <- tab.(j);
			tab.(j) <- temp
			
		let full_swap q i j =
			swap q.keys i j;
			swap q.priorities i j;
			swap q.mapping q.keys.(i) q.keys.(j)
			
		let left i = 2 * i + 1
		let right i = 2 * i + 2
		let parent i = (i - 1) / 2
			
		let rec sift_up q i =
			let j = parent i in
			if i > 0 && q.priorities.(i) < q.priorities.(j) then begin
				full_swap q i j;
				sift_up q j
			end
			
		let length q = q.last + 1
		
		let capacity q = Array.length q.keys
		
		let insert q (x, prio) =
			if length q = capacity q then failwith "insert"
			else begin
				let l = q.last + 1 in
				q.keys.(l) <- x;
				q.priorities.(l) <- prio;
				q.mapping.(x) <- l;
				q.last <- l;
				sift_up q q.last
			end
			
		let rec sift_down q i =
			let prio = q.priorities in
			let smallest = ref i in
			if left i <= q.last && prio.(left i) < prio.(i) then
				smallest := left i;
			if right i <= q.last && prio.(right i) < prio.(!smallest) then
				smallest := right i;
			if !smallest <> i then begin
				full_swap q i !smallest;
				sift_down q !smallest
			end
			
		let extract_min q =
			if q.last < 0 then
				failwith "extract_min"
			else
				begin
				let key = q.keys.(0) in
				let prio = q.priorities.(0) in
				full_swap q 0 q.last;
				q.mapping.(key) <- -1;
				q.last <- q.last - 1;
				sift_down q 0;
				key, prio
				end
				
		let decrease_priority q (x, prio) =
			let i = q.mapping.(x) in
			assert (mem q x && prio <= q.priorities.(i));
			q.priorities.(i) <- prio;
			sift_up q i
	end
;;

type arbre =
	|Feuille of char
	|Noeud of arbre*arbre
;;

type bitstream = bool list;;

let string_of_char_list u = String.of_seq (List.to_seq u);;

type table_code = bitstream array;;

let rec affiche_bitstream (a:bitstream) :unit = match a with
	|[]->()
	|b::q->if b then print_int 1 else print_int 0;
	affiche_bitstream q;;

let rec affiche_texte (l:char list) : unit = match l with 
	|[]->()
	|b::q->print_char b;
	affiche_texte q;;

let mon_arbre = Noeud(Noeud(Noeud(Feuille 'a',Feuille 'b'),Feuille 'd'),Feuille 'c');;

let rec decode_caractere (abr:arbre) (code:bitstream) :char*bitstream = match abr with
	|Feuille a -> (a,code)
	|Noeud (g,d) ->if not (List.hd (code)) then (decode_caractere (g) (List.tl code)) else (decode_caractere (d) (List.tl code));;

let rec decode (abr:arbre) (code:bitstream) : string = match code with
	|[]->""
	|a -> let (cractere, new_code) = decode_caractere abr a in
	(Char.escaped cractere )^(decode (abr) (new_code));;

print_string (decode (mon_arbre) ([false;false;true;false;true;true;false;false;false;true;false;true;false;false;true]));;

let cree_table (abr:arbre) :table_code = 
	let table = Array.make 256 [] in
	let rec aux (a:arbre) (code:bitstream) : bitstream=
		match a with 
		|Feuille c -> begin 
			table.(int_of_char c) <- code;
			[];
		end
		|Noeud (g,d) -> begin aux (g) ((code)@([false]));
		 aux (d) ((code)@([true]))
		end in
	let b = aux (abr) ([]) in
	table;;



let rec table_encode (table:table_code) (s:string) : bitstream = 
	if String.length<>1 then (table.(int_of_char s.[0]) @ (table_encode (table) (String.sub (s) (1) (String.length (s) -1))))
	else table.(int_of_char s.[0]);;

affiche_bitstream(table_encode (cree_table (mon_arbre)) ("abcd"));;