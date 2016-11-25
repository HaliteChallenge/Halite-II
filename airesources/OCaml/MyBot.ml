(* OCaml Starter for Halite on Halite.io
   This code is public domain. There is no warranty.
 *)

open Td;;

let random_move state row col =
  let dir = Halite.direction_of_int (Random.int 5) in
  let loc = {
    row = row;
    col = col;
  } in
    {
      loc = loc;
      direction = dir;
    }
;;

let random_moves state =
  let moves = ref [] in
    Array.iteri (fun ir row -> Array.iteri (fun ic site ->
      if site.owner = state.my_id then
        moves := (random_move state ir ic) :: !moves
    ) row) state.game_map.contents;
    !moves
;;

let mybot_function state =
  begin try
   (
    Halite.get_init state;
    Halite.send_init "MyOCamlBot";
    while true do
      Halite.get_frame state;
      let moves = random_moves state in
        Halite.send_frame moves
    done
   )
  with exc ->
   (
    Debug.debug (Printf.sprintf
       "Exception in turn %d :\n" state.round);
    Debug.debug (Printexc.to_string exc);
    raise exc
   )
  end;
;;

let run_bot bot =
  let game_state = Halite.init () in
    bot game_state
;;

run_bot mybot_function

