type unirtrouver = int array;;

let init (n:int) :unirtrouver = Array.init n (fun x->x);;

let trouver (u:unirtrouver) (i:int) : int = 
  let n = Array.length u in
  assert ((0<=i) && (i<n));
  u.(i);;

let unir (u:unirtrouver) (i:int) (j:int) :unit= 
  let n = Array.length u in
  assert ((0<=i) && (0<=j) && (i<n) && (j<n));
  let ancien = u.(j) in
  for k=0 to (n)-1 do
    if u.(k)=ancien then u.(k) <- u.(i);
  done;;


let u:unirtrouver = init 4;;
trouver u 3;;
unir u 0 3;;
trouver u 3;;
