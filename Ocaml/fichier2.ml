open fichier1

Random.self_init();

type image = int array array;;
let nouvelle_image (i:int): image = Array.init i (fun x->( Array.init i (fun x ->Random.int 2)));;



let afficher_image_nb (im:image) :unit = 
  let n = Array.length im in
  for k = 0 to n-1 do
    for r = 0 to n-1 do
      if im.(k).(r)=1 then print_char '.' else print_char '#';
    done;
    print_char '\n'
  done;;

afficher_image_nb (nouvelle_image (10));;

let identifiant (n:int) (i:int) (j:int) :int = i*n+j;; (*injective car 0<=j<n et  *)

let composantes (im:image) : unirtrouver = 
  let n = Array.length im in
  let u = init ((n)*(n)) in
  for i=0 to n-1 do
    for j=0 to n-1 do
      if i<>0 then
        (if im.(i).(j)=im.(i-1).(j) then unir u (n*i+j) (n*(i-1)+j));
      if j<>0 then
        (if im.(i).(j)=im.(i).(j) then unir u (n*i+j) (n*i+j-1));
    done;
  done;
  u;;

let recolore (im:image) (u:unirtrouver) :image = 
  let n = Array.length im in
  let colorier = nouvelle_image (n) in
  let table = Hashtbl.create 1 in
  for i = 0 to n-1 do
    for j = 0 to n-1 do if not (Hashtbl.mem ) (*a corriger*)
      Hashtbl.add (n*i+j) table u.(n*i+j)
    done;
  done;
  for i=0 to n-1 do
    for j=0 to n-1 do
      im.(i).(j) <-  char_of_int (Hashtbl.find table (n*i+j)); (*a corriger ici*)
    done;
  done;
  im;;




