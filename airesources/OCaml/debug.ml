(* OCaml Starter for Halite on Halite.io
   This code is public domain. There is no warranty.
 *)

let out_chan = (* stderr *) open_out "mybot_err.log" ;;

let debug s = 
  output_string out_chan s; 
  flush out_chan
;;

let error s = 
  output_string out_chan ("ERROR: " ^ s ^ "\n"); 
  flush out_chan
;;

(* Replace the functions above with these to silence all output 

let debug s = () ;;

let error s = () ;;

*)

