let n = int_of_string Sys.argv.(1) in
let f:in_channel = open_in "./pi_1_million.txt" in
for i=0 to n-2 do
  input_char f
done;
let l:char = input_char f in
Printf.printf "%c" l;;
close_in f;;