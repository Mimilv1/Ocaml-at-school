let f : in_channel = open_in "./entree.txt" in 
let g : out_channel = open_out "./sortie.txt" in

try
  while true do 
    output_string g ((input_line (f)));
    output_char g '\n';
    input_line f;
  done
with
  |End_of_file -> 
    close_out g;
    close_in f;