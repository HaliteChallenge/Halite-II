(* OCaml Starter for Halite II on Halite.io
   This code is public domain. There is no warranty.
 *)

open Td;;

let find_dockable_planet ship planets = 
  try
    Some (List.find (fun planet ->
      planet.p_entity.owner < 0
    ) planets)
  with Not_found -> None
;;

let debug_show_command c =
  Debug.debug (Printf.sprintf "%s\n" (Networking.serialize_move c))
;;

let initial_bot_setup state =
  (* Anything you want to do with your 60 seconds of pre-game time *)
  ()
;;

let settler_moves state =
  List.fold_left (fun acc ship ->
    if ship.status = `Undocked then (
      match find_dockable_planet ship state.planets with
      | None -> acc
      | Some planet ->
        if Game.can_dock ship planet then
          Game.cmd_dock ship planet :: acc
        else (
          let x, y = 
           Game.closest_point ship.s_entity planet.p_entity Const.ship_radius
          in
          let point = Game.position_entity x y in
          begin match Game.navigate 
            state ship point (Const.max_speed / 2) true 90 1. false false
          with
          | None -> acc
          | Some command -> 
            command :: acc
          end
        )
    )
    else acc
  ) [] (Game.get_me state).ships
;;

let mybot_function state =
  begin try
   (
    Game.init state "SettlerOcaml" initial_bot_setup;
    while true do
      Game.update_map state;
      let moves = settler_moves state in
        Game.send_command_queue moves
    done
   )
  with exc ->
   (
    Debug.debug (Printf.sprintf
       "Exception in turn %d :\n" state.turn);
    Debug.debug (Printexc.to_string exc);
    Debug.close_files ();
    raise exc
   )
  end;
;;

let run_bot bot =
  let state = Utility.init_state () in
    bot state
;;

run_bot mybot_function;;


