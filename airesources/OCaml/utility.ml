(* OCaml Starter for Halite II on Halite.io
   This code is public domain. There is no warranty.
 *)

open Td;;

let time_seconds_elapsed_this_turn state =
  Unix.gettimeofday () -. state.last_update
;;

let new_ship player_id ship_id x y hp vel_x vel_y 
  dock_status docked_planet progress cooldown 
=
  let entity =
   {
    id = ship_id;
    x = x;
    y = y;
    radius = Const.ship_radius;
    health = hp;
    owner = player_id;
   }
  in
   {
    s_entity = entity;
    status = dock_status;
    docked_planet_id = docked_planet;
    progress = progress;
    cooldown = cooldown;
   }
;;

let new_planet planet_id x y health r docking_spots production resources owned owner docked_ship_ids = 
  let entity = 
   {
    id = planet_id;
    x = x;
    y = y;
    radius = r;
    health = health;
    owner = if owned then owner else -1;
   }
  in
   {
    p_entity = entity;
    num_docking_spots = docking_spots;
    current_production = production;
    remaining_resources = resources;
    docked_ship_ids = docked_ship_ids;
    docked_ships = [];
   }
;;

let init_state () =
 {
  my_id = -1;
  width = -1;
  height = -1;
  player = [| |];
  planets = [];
  all_ships = [];
  turn = -1;
  last_update = 0.;
 }
;;


