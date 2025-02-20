open Map
open Item
open Character
open Game_state
open Selection
open Logging

type event = NoEvent | SafeEvent | JonesyEvent | XenoEvent

(** Get a random event, along with the morale delta *)
let random_event () : event * int =
  match Random.int_in_range ~min:1 ~max:12 with
  | x when x <= 8 ->
      (SafeEvent, 0)
  | x when x <= 10 && !game_state.jonesy_caught ->
      (SafeEvent, 0)
  | x when x <= 10 && not !game_state.jonesy_caught ->
      ( JonesyEvent
      , let carrier_chars =
          List.filter (character_has_item CatCarrier) !game_state.characters
        in
        if carrier_chars = [] then
          (* Jonesy is discovered but no characters can catch him *)
          1
        else
          match
            get_int_selection ~back_string:"No"
              (Printf.sprintf "%s %s a CatCarrier, use one to catch Jonesy?"
                 (String.concat ", "
                    (List.map (fun x -> x.last_name) carrier_chars) )
                 ( if List.length carrier_chars = 1 then
                     "has"
                   else
                     "have" ) )
              (List.map (fun x -> x.last_name) carrier_chars)
              true
          with
          | None ->
              (* Jonesy is discovered and the party owns a cat carrier, but does not use it *)
              1
          | Some idx ->
              (* Jonesy is discovered and the party owns a cat carrier and uses it *)
              let catcher = List.nth carrier_chars idx in
              remove_character_item catcher CatCarrier ;
              Printf.printf "%s used the %s to catch Jonesy\n" catcher.last_name
                (string_of_item CatCarrier) ;
              game_state := {!game_state with jonesy_caught= true} ;
              0 )
  | _ ->
      (XenoEvent, Random.int_in_range ~min:1 ~max:2)

(** Trigger a post-move event, return it *)
let trigger_event (active_character : character)
    (motion_tracker_room : room option) : event =
  let uses_motion_tracker, target_room =
    match motion_tracker_room with
    | None ->
        (false, locate_character active_character)
    | Some r ->
        (true, r)
  in
  let jonesy_caught = !game_state.jonesy_caught in
  if !game_state.has_event target_room then (
    let event = fst (random_event ()) in
    ( match event with
    | NoEvent ->
        ()
    | SafeEvent ->
        print_endline
          ( if uses_motion_tracker then
              "All seems quiet..."
            else
              "[EVENT] - Safe" )
    | JonesyEvent when jonesy_caught ->
        print_endline
          ( if uses_motion_tracker then
              "All seems quiet..."
            else
              "[EVENT] - Safe" )
    | JonesyEvent ->
        print_endline
          ( if uses_motion_tracker then
              "Something tiny makes a blip. Probably Jonesy"
            else
              "[EVENT] - Safe" )
    | XenoEvent when uses_motion_tracker ->
        print_endline "Something huge and fast. The Xenomorph?" ;
        game_state :=
          { !game_state with
            xeno_room=
              ( match
                  List.find_index
                    (fun r -> r.name = target_room.name)
                    !game_state.map.rooms
                with
              | None ->
                  _log Log_Error
                    "Couldn't place xenomorph in motion tracker room" ;
                  !game_state.xeno_room
              | Some x ->
                  x ) }
    | XenoEvent ->
        print_endline "[EVENT] - Surprise attack!" ;
        print_endline "You encounter the Xenomorph!" ;
        game_state :=
          { !game_state with
            xeno_room=
              ( match
                  List.find_index
                    (fun r -> r.name = target_room.name)
                    !game_state.map.rooms
                with
              | None ->
                  _log Log_Error
                    "Couldn't place xenomorph in motion tracker room" ;
                  !game_state.xeno_room
              | Some x ->
                  x ) } ) ;
    event
  ) else
    NoEvent
