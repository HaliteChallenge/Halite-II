(* OCaml Starter for Halite on Halite.io
   This code is public domain. There is no warranty.
 *)

open Td;;

let mybot_function state =
  if not !Halite.sent_init then
    (* Setup turn, put setup logic here *)
    Halite.send_init "MyOCamlBot"
  else (
    (* put normal game logic here, replacing the random-bot code below *)
    let moves = ref [] in
    Array.iteri (fun ir row -> Array.iteri (fun ic site ->
      if site.owner = state.my_id then
        moves := (Halite.random_move state ir ic) :: !moves
    ) row) state.game_map.contents;
    Halite.send_frame(!moves)
  )
;;

Halite.run_bot mybot_function

