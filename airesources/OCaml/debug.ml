(* OCaml Starter for Halite II on Halite.io
   This code is public domain. There is no warranty.
 *)

let out_chan = ref stdout;;

let file_left_open = ref false;;

let setup_logging tag name =
  let filename = Printf.sprintf "%i_%s.log" tag name in
    out_chan := open_out filename;
    file_left_open := true
;;

let close_files () =
  if !file_left_open then
    close_out !out_chan
;;

let debug s = 
  output_string !out_chan s; 
  flush !out_chan
;;

let info s = 
  output_string !out_chan s; 
  flush !out_chan
;;

let error s = 
  output_string !out_chan ("ERROR: " ^ s ^ "\n"); 
  flush !out_chan
;;

(* Replace the functions above with these to silence all logging.
 * Individual functions can be silenced selectively.

let debug s = () ;;

let info s = () ;;

let error s = () ;;

*)
