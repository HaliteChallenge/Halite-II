(* OCaml Starter for Halite II on Halite.io
   This code is public domain. There is no warranty.
 *)

open Td;;

let pi = 3.14159265358979312;;

let deg2rad v = (v *. pi) /. 180.

let rad2deg v = (v /. pi) *. 180.

let set_planets_docked_ships state =
  List.iter (fun planet ->
    planet.docked_ships <- List.filter (fun ship ->
      ship.status = `Docked
      && ship.docked_planet_id = planet.p_entity.id
    ) state.all_ships
  ) state.planets
;;

let update_map state =
  state.turn <- state.turn + 1;
  state.last_update <- Unix.gettimeofday ();
  Debug.info (Printf.sprintf "--- Begin Turn %i ---\n" state.turn);
  Networking.parse_map state (Networking.get_string());
  set_planets_docked_ships state;
;;

let init state name initial_bot_setup =
  state.my_id <- int_of_string (Networking.get_string ());
  Debug.setup_logging state.my_id name;
  Networking.read_width_height state (Networking.get_string ());
  update_map state;
  initial_bot_setup state;
  Networking.send_string name;
  Networking.done_sending (); 
;;

let send_command_queue =
  Networking.send_moves 
;;

let get_me state =
  state.player.(state.my_id)
;;

let get_player state id =
  state.player.(id)
;;

let all_players state =
  state.player
;;

let get_planet state id =
  List.find (fun planet ->
    planet.p_entity.id = id
  ) state.planets
;;

let all_planets state =
  state.planets
;;

let planet_entities planets = 
  List.map (fun planet -> planet.p_entity) planets
;;

let ship_entities ships =
  List.map (fun ship -> ship.s_entity) ships
;;

let all_ship_entities state =
  ship_entities state.all_ships
;;

let all_entities state =
  planet_entities state.planets
  @ all_ship_entities state
;;

let all_entities_except state entity =
  List.filter (fun e ->
    not (e == entity)
  ) (all_entities state)
;;

let calculate_distance_between e1 e2 =
  sqrt ((e1.x -. e2.x) ** 2. +. (e1.y -. e2.y) ** 2.)
;;

let calculate_radians_between e1 e2 =
  (atan2 (e2.y -. e1.y) (e2.x -. e1.x))
;;

let calculate_angle_between e1 e2 =
  mod_float (rad2deg (atan2 (e2.y -. e1.y) (e2.x -. e1.x))) 360.
;;

let closest_point from_entity to_entity min_distance =
  let angle = calculate_radians_between to_entity from_entity in
  let radius = to_entity.radius +. min_distance in
  let x = to_entity.x +. radius *. (cos angle) in
  let y = to_entity.y +. radius *. (sin angle) in
    x, y
;;

let nearby_entities_by_distance state entity =
  List.map (fun e2 ->
    e2, (calculate_distance_between entity e2)
  ) (all_entities_except state entity)
;;

let all_ships state =
  state.all_ships
;;

let docked_ships planet =
  planet.docked_ships
;;

let get_planet_docked_ship planet ship_id =
  try
    Some (List.find (fun ship ->
      ship.s_entity.id = ship_id
    ) planet.docked_ships)
  with _ -> None
;;

let planet_is_owned planet =
  planet.p_entity.owner >= 0
;;

let planet_is_full planet =
  List.length planet.docked_ship_ids >= planet.num_docking_spots
;;

let positive_int_degrees a =
  ((a mod 360) + 360) mod 360
;;

let cmd_thrust ship magnitude a =
  let angle = positive_int_degrees (int_of_float (floor (a +. 0.5))) in
    `Thrust (ship.s_entity.id, magnitude, angle)
;;

let cmd_dock ship planet =
  `Dock (ship.s_entity.id, planet.p_entity.id)
;;

let cmd_undock ship =
  `Undock ship.s_entity.id
;;

let can_dock ship planet =
  let distance = calculate_distance_between ship.s_entity planet.p_entity in
    distance <= planet.p_entity.radius +. Const.dock_radius +. Const.ship_radius
;;

let position_entity x y =
 {
  id = (-1);
  x = x;
  y = y;
  radius = 0.1;
  health = 0;
  owner = (-1)
 }
;;

let intersect_segment_circle start eend circle fudge =
  let dx = eend.x -. start.x in
  let dy = eend.y -. start.y in
  let a = dx ** 2. +. dy ** 2. in
  let b = (-2.) *. 
    (start.x ** 2. -. 
      start.x *. eend.x -. 
      start.x *. circle.x +. 
      eend.x *. circle.x +. 
     start.y ** 2. -. 
     start.y *. eend.y -. 
     start.y *. circle.y +. 
     eend.y *. circle.y) 
  in
    if a = 0. then (
      calculate_distance_between start circle <= circle.radius +. fudge
    )
    else (
      let t = min (-.b /. (2. *. a)) 1. in
        if t < 0. then false
        else (
          let closest_x = start.x +. dx *. t in
          let closest_y = start.y +. dy *. t in
          let pos_ent = position_entity closest_x closest_y in
          let closest_distance = calculate_distance_between pos_ent circle in
            closest_distance <= circle.radius +. fudge
        )
    )
  
;;


(* Note that unlike the Python starter, this obstacles_between function
 * expects a list of potential obstacles, rather than a list of types
 * to exclude. It is up to the calling function to filter the obstacles.
 *)
let obstacles_between e1 e2 obstacles =
  List.filter (fun obst ->
    (not ((obst == e1) || (obst == e2)))
    && intersect_segment_circle e1 e2 obst (Const.ship_radius +. 0.1)
  ) obstacles
;;

let direct_navigate ship distance max_speed angle =
  let speed = int_of_float (min (float_of_int max_speed) distance) in
    cmd_thrust ship speed angle
;;

let rec navigate state ship target speed avoid max_correct angular_step ignore_ships (ignore_planets:bool) =
  if max_correct <= 0 then None
  else (
    let distance = calculate_distance_between ship.s_entity target in
    let angle = calculate_angle_between ship.s_entity target in
    if avoid then (
      let obstacles = 
        let planet_entities = 
          if ignore_planets then [] else planet_entities state.planets
        in
        let ship_entities =
          if ignore_ships then [] else all_ship_entities state
        in
          ship_entities @ planet_entities
      in
        let problems = obstacles_between ship.s_entity target obstacles in
          if List.length problems > 0 then (
            let new_target_dx = cos (deg2rad (angle +. angular_step)) *. distance in
            let new_target_dy = sin (deg2rad (angle +. angular_step)) *. distance in
            let new_target = position_entity (ship.s_entity.x +. new_target_dx) (ship.s_entity.y +. new_target_dy) in
              navigate state ship new_target speed avoid (max_correct - 1) angular_step ignore_ships ignore_planets
          )
          else Some (direct_navigate ship distance speed angle)
    ) else (
      Some (direct_navigate ship distance speed angle)
    )

  )
;;



