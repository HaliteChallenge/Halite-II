(* OCaml Starter for Halite on Halite.io
   This code is public domain. There is no warranty.
 *)

type t_direction = [ `Still | `North | `East  | `South | `West ] ;;

type location = 
 {
  mutable row : int;
  mutable col : int;
 }
;;

type site =
 {
  mutable owner : int;
  mutable strength : int;
  mutable production : int;
 }
;;

type move =
 {
  mutable loc : location;
  mutable direction : t_direction;
 }
;;

type game_map =
 {
  mutable width : int;
  mutable height : int;
  mutable contents : site array array;
 }
;;

type game_state =
 {
  mutable my_id : int;
  mutable round : int;
  mutable max_rounds : int;
  mutable last_update : float;
  mutable game_map : game_map;
 }
;;


