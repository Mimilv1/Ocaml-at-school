let arg1:string = Sys.argv.(1) in
let arg2:string =Sys.argv.(2) in
let taille :int = String.length arg1 + String.length arg2 in
Printf.printf "%d" taille;;
Printf.printf "\n";;