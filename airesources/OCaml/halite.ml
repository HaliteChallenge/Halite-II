(* OCaml Starter for Halite on Halite.io
   This code is public domain. There is no warranty.
 *)

open Td;;
open Debug;;

(* Some useful things *)

let time_first_turn = 15000.0;;
let time_per_turn = 1000.0;;
let list_directions = [`Still; `North; `East; `South; `West];;
let list_cardinals = [`North; `East; `South; `West];;
let (array_directions:t_direction array) = 
  [| `Still; `North; `East; `South; `West |]
;;
let (array_cardinals:t_direction array) = [| `North; `East; `South; `West |];;


(* Some boolean reference cells to track init status *)
let first_turn = ref false;;
let init_my_id_done = ref false;;
let init_size_done = ref false;;
let init_production_done = ref false;;
let init_map_done = ref false;;
let all_init_done = ref false;;
let sent_init = ref false;;

let get_time () = Unix.gettimeofday ();;

(* input processing *)

let new_site () = 
 {
  owner = -1;
  strength = -1;
  production = -1;
 }
;;

let new_blank_game_map width height =
  let v = Array.make_matrix height width 0 in
    Array.map (Array.map (fun _ -> new_site())) v
;;

let clear_state state =
  state.game_map.contents <- 
    new_blank_game_map state.game_map.width state.game_map.height;
  state.round <- 0;
;;

(* tokenizer from rosetta code *)
let split_char sep str =
  let string_index_from i =
    try Some (String.index_from str i sep)
    with Not_found -> None
  in
  let rec aux i acc = match string_index_from i with
    | Some i' ->
        let w = String.sub str i (i' - i) in
        aux (succ i') (w::acc)
    | None ->
        let w = String.sub str i (String.length str - i) in
        List.rev (w::acc)
  in
  aux 0 []
;;

let set_owner state coord owner =
  let row = coord / state.game_map.width in
  let col = coord mod state.game_map.width in
    state.game_map.contents.(row).(col).owner <- owner
;;

let set_strength state coord v =
  let row = coord / state.game_map.width in
  let col = coord mod state.game_map.width in
    state.game_map.contents.(row).(col).strength <- v
;;

let set_production state coord v =
  let row = coord / state.game_map.width in
  let col = coord mod state.game_map.width in
    state.game_map.contents.(row).(col).production <- v
;;

let set_owners state start repeat owner =
  for coord = start to (start + repeat - 1) do
    set_owner state coord owner
  done
;;

let total_size state = state.game_map.width * state.game_map.height;;

let deserialize_map state tokens =
  let count = ref 0 in
  let repeat = ref 0 in
  let ready = ref false in
  let num_sites = total_size state in
  List.iter (fun token ->
    if !count < num_sites then (
      if !ready then (
        set_owners state !count !repeat (int_of_string token);
        count := !count + !repeat;
        ready := false;
      ) else (
        repeat := int_of_string token;
        ready := true;
      )
    )
    else (
      set_strength state (!count mod num_sites) (int_of_string token);
      count := !count + 1;
    )
  ) tokens
;;

let init_production state tokens =
  List.iteri (fun i s ->
    set_production state i (int_of_string s)
  ) tokens
;;

let init_size state tokens =
  match tokens with
  | w :: h :: [] ->
    let width = int_of_string w in
    let height = int_of_string h in
    state.game_map.width <- width;
    state.game_map.height <- height;
    state.max_rounds <- (int_of_float (sqrt (float_of_int(width * height)))) * 10;
    clear_state state;
  | _ -> 
    let s = 
      List.fold_left (fun acc t -> acc ^ " " ^ t) " " tokens 
    in
    failwith ("incorrect input for init_size " ^ s ^ "\n")
;;

let init_my_id state tokens =
  match tokens with
  | id :: [] ->
    state.my_id <- int_of_string id
  | _ -> failwith ("incorrect input for init_my_id\n")
;;

let process_line bot state line =
  let tokens = split_char ' ' (String.trim line) in
    if !all_init_done then (
      deserialize_map state tokens;
      bot state;
    ) else if not !init_my_id_done then (
      init_my_id state tokens;
      init_my_id_done := true;

    ) else if not !init_size_done then (
      init_size state tokens;
      init_size_done := true;

    ) else if not !init_production_done then (
      init_production state tokens;
      init_production_done := true;

    ) else if not !init_map_done then (
      deserialize_map state tokens;
      init_map_done := true;
      all_init_done := true;
      bot state;
    )

    else Debug.error "Impossible condition: all_init_done not set when all init done"

;;

let read_lines bot state =
  while true do
    let line = read_line () in
      process_line bot state line;
  done
;;

(* End input section *)

(* output section *)

let char_of_dir = function
| `Still -> '0'
| `North -> '1'
| `East -> '2'
| `South -> '3'
| `West -> '4'
;;

let serialize_move move =
  Printf.sprintf "%i %i %c" move.loc.col move.loc.row (char_of_dir move.direction)
;;

let send_frame moves =
  List.iter (fun move ->
    Printf.printf "%s " (serialize_move move)
  ) moves;
  Printf.printf "\n";
  flush stdout;
;;

let send_init name =
  print_string (name ^ "\n");
  flush stdout;
  sent_init := true;
;;

(* End output section *)

(* Utility functions *)

let random_from_list lst =
  let len = List.length lst in
    List.nth lst (Random.int len)
;;

let in_bounds state row col =
  (row >= 0) && (col >= 0)
  && 
  (row < state.game_map.height) && (col < state.game_map.width)
;;

let get_distance state l1 l2 =
  let pd_col = abs (l1.col - l2.col) in
  let pd_row = abs (l1.row - l2.row) in
  let d_col = 
    if pd_col > state.game_map.width / 2 then state.game_map.width - pd_col else pd_col
  in
  let d_row = 
    if pd_row > state.game_map.height / 2 then state.game_map.height - pd_row else pd_row
  in
    d_row + d_col
;;

let get_angle state l1 l2 =
  let pd_col = l2.col - l1.col in
  let pd_row = l2.row - l1.row in
  let d_col =
    if pd_col > state.game_map.width - pd_col then pd_col - state.game_map.width
    else if (-pd_col) > state.game_map.width + pd_col then
      pd_col + state.game_map.width
    else pd_col
  in
  let d_row =
    if pd_row > state.game_map.height - pd_row then 
      pd_row - state.game_map.height
    else if (-pd_row) > state.game_map.height + pd_row then
      pd_row + state.game_map.height
    else pd_row
  in
    atan2 (float_of_int d_row) (float_of_int d_col)
;;

let wrap_loc state loc =
  let wrap v bound =
    if v < 0 then bound + v
    else if v >= bound then v - bound
    else v
  in
  let w_row = wrap loc.row state.game_map.height in
  let w_col = wrap loc.col state.game_map.width in
    {row = w_row; col = w_col}
;;

let get_location state loc direction =
  let (o_row, o_col) =
    match direction with
    | `Still -> (0, 0)
    | `North -> (-1, 0)
    | `East -> (1, 0)
    | `South -> (0, -1)
    | `West -> (0, 1)
  in
  let row = loc.row + o_row in
  let col = loc.col + o_col in
    wrap_loc state {row = row; col = col}
;;

let get_site state loc direction =
  let l = get_location state loc direction in
    state.game_map.contents.(l.row).(l.col)
;;

let time_elapsed_this_turn state =
  (get_time() -. state.last_update) *. 1000.
;;

let time_remaining state =
  let turntime = if !first_turn then time_first_turn else time_per_turn in
  (turntime -. time_elapsed_this_turn state)
;;

let direction_of_int = function
| 0 -> `Still
| 1 -> `North
| 2 -> `East
| 3 -> `South
| 4 -> `West
| n -> failwith ("Invalid direction_of_int: " ^ (string_of_int n) ^ "\n")
;;

let random_move state row col =
  let dir = direction_of_int (Random.int 5) in
  let loc = {
    row = row;
    col = col;
  } in
    {
      loc = loc;
      direction = dir;
    }
;;

let debug_block state f =
  Array.iter (fun row ->
    Debug.debug "\n";
    Array.iter (fun site -> 
      Debug.debug (f site)
    ) row
  ) state.game_map.contents;
  Debug.debug "\n"
;;

let debug_productions state =
  debug_block state (fun site -> 
    Printf.sprintf "%2d" site.production
  )
;;

let debug_owners state =
  debug_block state (fun site -> 
    (string_of_int site.owner) ^ " "
  )
;;

let debug_strength state =
  debug_block state (fun site -> 
    Printf.sprintf "%3d " site.strength
  )
;;

let debug_game_map state =
  debug_owners state;
  debug_strength state;
  debug_productions state;
;;

(* End utility *)

let init () =
  Random.self_init (); 
  let gmap =
   {
    width = (-1);
    height = (-1);
    contents = [|[| |]|];
   }
  in
  let state =
   {
    my_id = (-1);
    round = 0;
    max_rounds = 0;
    last_update = (-1.0);
    game_map = gmap;
   }
  in
    state 
;;

let feed_bot bot state line =
  process_line bot state line;
;;

let run_bot bot =
  let game_state = init () in

  begin try
   (
     read_lines bot game_state
   )
  with exc ->
   (
    debug (Printf.sprintf
       "Exception in turn %d :\n" game_state.round);
    debug (Printexc.to_string exc);
    raise exc
   )
  end;
;;


