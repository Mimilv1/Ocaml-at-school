
let n = int_of_string Sys.argv.(1) in
let f:in_channel = open_in "./pi.txt" in
for i=0 to 999999 do
  