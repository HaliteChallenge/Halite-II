(* OCaml Starter for Halite II on Halite.io
   This code is public domain. There is no warranty.
 *)


type docking_status = [ `Undocked | `Docking | `Docked | `Undocking ];;

type move = [ 
  | `Thrust of (int * int * int)
  | `Dock of (int * int)
  | `Undock of int
]

type entity = 
 {
  mutable id : int;
  mutable x : float;
  mutable y : float;
  mutable radius : float;
  mutable health : int;
  mutable owner : int;
 }
;;

type ship =
 {
  mutable s_entity : entity;
  mutable status : docking_status;
  mutable docked_planet_id : int;
  mutable progress : int;
  mutable cooldown : int;
 }
;;

type planet =
 {
  mutable p_entity : entity;
  mutable num_docking_spots : int;
  mutable current_production : int;
  mutable remaining_resources : int;
  mutable docked_ship_ids : int list;
  mutable docked_ships : ship list;
 }
;;

(* Single type for both ships and planets *)
type feature =
 [ `Ship of ship | `Planet of planet ]
;;

type player =
 {
  mutable player_id : int;
  mutable ships: ship list;
 }
;;

type game_state =
 {
  mutable my_id : int;
  mutable width : int;
  mutable height : int;
  mutable player : player array;
  mutable planets : planet list;
  mutable all_ships : ship list;
  mutable turn : int;
  mutable last_update : float;
 }
;;

