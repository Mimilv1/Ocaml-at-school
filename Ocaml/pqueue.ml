type 'a pqueue = {mutable size : int; mutable data : ('a * int) array}

exception Empty

let create() =
	{size = 0 ; data = [||]}

let print (p : 'a pqueue) (print_a : 'a -> unit) =
	for i = 0 to p.size-1 do
		let (x,k) = p.data.(i) in
		Printf.printf "(" ;
		print_a x ;
		Printf.printf ",%d)-" k
	done ;
	Printf.printf "\n"
		
let is_empty (p : 'a pqueue) =
	p.size = 0

let push (x : 'a) (k : int) (p : 'a pqueue) =	
	(* on redimensionne si nécessaire *)
	if p.size = Array.length p.data then
		begin
		let t = Array.init (if p.size = 0 then 1 else 2*p.size) (fun i -> if i < p.size then p.data.(i) else (x,k)) in
		p.data <- t;
		end;
	(* on ajoute l'élément à la bonne position *)
	p.data.(p.size) <- (x,k) ;
	p.size <- p.size+1 ;
	let rec remonter i =
		if i > 0 then
			begin
			let (y,k1) = p.data.(i) in
			let (z,k2) = p.data.((i-1)/2) in
			if k1 < k2 then
				begin
				p.data.(i) <- (z,k2) ;
				p.data.((i-1)/2) <- (y,k1) ;
				remonter ((i-1)/2)
				end
			end
	in remonter (p.size-1)

let pop (p : 'a pqueue) : 'a =
	if is_empty p then 
		raise Empty
	else
	begin
	p.size <- p.size-1 ;
	let (x,k) = p.data.(0) in
	let (a,b) = p.data.(p.size) in
	p.data.(0) <- (a,b) ;
	p.data.(p.size) <- (x,k) ;
	let rec descendre i =
		if 2*i+1 < p.size then
			begin
			let (y,k1) = p.data.(i) in
			let (z,k2) = p.data.(2*i+1) in
			if 2*i+2 < p.size then
				begin
				let (u,k3) = p.data.(2*i+2) in
				let mink = min k2 k3 in
				let minx = (if k2 < k3 then z else u) in
				let minpos = 2*i+1 + (if k2 < k3 then 0 else 1) in
				if k1 > mink then
					begin
					p.data.(i) <- (minx,mink) ;
					p.data.(minpos) <- (y,k1) ;
					descendre minpos
					end
					
				end
			else
				if k1 > k2 then
				begin
				p.data.(i) <- (z,k2) ;
				p.data.(2*i+1) <- (y,k1) ;
				descendre (2*i+1)
				end
			end
	in descendre 0 ;
	x
	end

let tri_par_tas (t : int array) =
	let p = create() in
	for i = 0 to (Array.length t) -1 do
		push t.(i) t.(i) p
	done ;
	for i = 0 to (Array.length t)-1 do
		t.(i) <- pop p
	done
