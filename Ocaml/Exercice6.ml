let tab:int array = [|0;0;0;0;0;0;0;0;0;0|] in
let n = int_of_string Sys.argv.(1) in
let f:in_channel = open_in "./pi.txt" in
for i=0 to n-1 do
  let l:int = int_of_char (input_char (f)) - 48 in
  tab.(l) <- tab.(l) + 1;
done;




Printf.printf "il y a %d fois le nombre 0 \n" tab.(0);
Printf.printf "il y a %d fois le nombre 1 \n" tab.(1);
Printf.printf "il y a %d fois le nombre 2 \n" tab.(2);
Printf.printf "il y a %d fois le nombre 3 \n" tab.(3);
Printf.printf "il y a %d fois le nombre 4 \n" tab.(4);
Printf.printf "il y a %d fois le nombre 5 \n" tab.(5);
Printf.printf "il y a %d fois le nombre 6 \n" tab.(6);
Printf.printf "il y a %d fois le nombre 7 \n" tab.(7);
Printf.printf "il y a %d fois le nombre 8 \n" tab.(8);
Printf.printf "il y a %d fois le nombre 9 \n" tab.(9);
close_in f;;