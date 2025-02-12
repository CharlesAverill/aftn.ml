open Game_state
open Map
open Character
open Ripley
open Dallas
open Parker
open Brett
open Lambert
open Selection
open Random
open Objective
open Logging

(** Set up the game state given user preferences *)
let setup_game (n_players : int) (n_characters : int) (use_ash : bool) : unit =
  (* Set random seed *)
  Random.self_init () ;
  game_state :=
    { !game_state with
      morale=
        ( if n_characters > 3 then
            20
          else
            15
          (* Set up initial morale *) )
    ; xeno_room= !game_state.map.xeno_start_room (* Place xenomorph *)
    ; ash_room=
        ( if use_ash then
            Some !game_state.map.ash_start_room
          else
            None
          (* Place Ash *) )
    ; map=
        { !game_state.map with
          rooms=
            List.fold_left
              (fun a r ->
                let num_scrap = ref r.num_scrap in
                let has_event = ref r.has_event in
                let has_coolant = ref r.has_coolant in
                if List.exists (fun x -> x = r) !game_state.map.scrap_rooms then
                  num_scrap := 2 ;
                if List.exists (fun x -> x = r) !game_state.map.event_rooms then
                  has_event := true ;
                if List.exists (fun x -> x = r) !game_state.map.coolant_rooms
                then
                  has_coolant := true ;
                { r with
                  num_scrap= !num_scrap
                ; has_event= !has_event
                ; has_coolant= !has_coolant }
                :: a )
              [] !game_state.map.rooms }
        (* Place initial scrap *) } ;
  if n_characters = 5 then (
    add_character ripley !game_state.map.player_start_room ;
    add_character dallas !game_state.map.player_start_room ;
    add_character parker !game_state.map.player_start_room ;
    add_character brett !game_state.map.player_start_room ;
    add_character lambert !game_state.map.player_start_room
  ) else
    let char_options = ref [ripley; dallas; parker; brett; lambert] in
    let selected = ref 0 in
    Printf.printf "Select %d character%s:\n" n_characters
      ( if n_characters > 1 then
          "s"
        else
          "" ) ;
    while !selected < n_characters do
      if !selected > 0 then
        print_endline
          ( "Selected characters: "
          ^ String.concat ", "
              (List.map (fun c -> c.last_name) !game_state.characters) ) ;
      match
        get_int_selection
          "Which character would you like to add? Select b) to exit"
          (List.map string_of_character !char_options)
          true
      with
      | None ->
          exit 0
      | Some n ->
          add_character (List.nth !char_options n)
            !game_state.map.player_start_room ;
          selected := !selected + 1 ;
          char_options :=
            List.filter
              (fun c -> ch_neq c (List.nth !char_options n))
              !char_options
    done ;
    game_state :=
      { !game_state with
        objectives=
          List.map
            (fill_in_obj_room !game_state.map)
            (get_objectives (n_characters + 1))
          (* Get random objectives *) } ;
    shuffle_encounters ()

(** TODO : implement *)
let update_objectives () = ()

(** Possible turn actions *)
type action =
  | Move
  | PickUp
  | Drop
  | Ability
  | ViewInventory
  | ViewTeam
  | Craft
  | UseItem
  | GiveItem
  | EndTurn
  | ViewRoom
  | ViewCharacterLocations
  | SeeObjectives
  | DrawMap
  | Exit

(** Get printable [string] representation of an [action] *)
let string_of_action = function
  | Move ->
      "Move"
  | PickUp ->
      "Pick up"
  | Drop ->
      "Drop"
  | Ability ->
      "Use ability"
  | ViewInventory ->
      "View inventory"
  | ViewTeam ->
      "View team"
  | Craft ->
      "Craft item"
  | UseItem ->
      "Use item"
  | GiveItem ->
      "Give item"
  | EndTurn ->
      "End turn early"
  | ViewRoom ->
      "View current room"
  | ViewCharacterLocations ->
      "View team locations"
  | SeeObjectives ->
      "View objectives"
  | DrawMap ->
      "Draw map"
  | Exit ->
      "Exit"

(** List of actions to choose from *)
let actions =
  [ Move
  ; PickUp
  ; Drop
  ; Ability
  ; ViewInventory
  ; ViewTeam
  ; Craft
  ; UseItem
  ; GiveItem
  ; EndTurn
  ; ViewRoom
  ; ViewCharacterLocations
  ; SeeObjectives
  ; DrawMap
  ; Exit ]

(** Prompt player for room to move to *)
let character_move (active_character : character) (allowed_moves : room list)
    (allow_back : bool) : room option =
  let ladder =
    if allowed_moves = [] then
      (locate_character active_character).ladder_connection
    else
      None
  in
  let allowed_moves =
    ( if allowed_moves = [] then
        (locate_character active_character).connections
      else
        List.map (fun r -> r.name) allowed_moves )
    @ match ladder with None -> [] | Some r -> [r.name]
  in
  update_objectives () ;
  match
    get_int_selection "Destinations:"
      (List.map
         (fun r ->
           r
           ^ match ladder with Some x when x.name = r -> " (ladder)" | _ -> "" )
         allowed_moves )
      allow_back
  with
  | None ->
      None
  | Some i ->
      find_room !game_state.map (List.nth allowed_moves i)

(** Main game loop *)
let game_loop () : unit =
  print_endline
    "|==============================================|\n\
     |==============SITUATION CRITICAL==============|\n\
     |========REPORT ISSUED BY DALLAS, ARTHUR=======|\n\
     |An Alien is stalking us on board the          |\n\
     |Nostromo, and Executive Officer Kane is       |\n\
     |dead. The remaining crew and I are working    |\n\
     |together to patch the ship and do what we     |\n\
     |can to survive. I don't know if we'll make    |\n\
     |it. The Alien is big, fast, and deadly, and   |\n\
     |could be waiting just beyond the next hatch...|\n\
     |==============================================|\n\
     |==============================================|\n" ;
  print_endline "Press enter to continue" ;
  let _ = read_line () in
  let broken = ref false in
  while not !broken do
    Printf.printf "=====Round %d=====\n" !game_state.round_count ;
    (* Character turns *)
    List.iteri
      (fun i active_character ->
        game_state :=
          {!game_state with turn_idx= i; active_character= Some active_character} ;
        Printf.printf "-----Turn %d-----\n" !game_state.turn_idx ;
        match !game_state.self_destruct_count with
        | Some x when i = 0 ->
            game_state := {!game_state with self_destruct_count= Some (x - 1)} ;
            if x - 1 <= 0 then (
              print_endline
                "[SELF-DESTRUCT] The Self-Destruct timer drops to 0!" ;
              print_endline
                "[GAME OVER] - The Nostromo self-destructed with the Crew \
                 still on it!" ;
              exit 0
            ) else
              Printf.printf
                "[SELF-DESTRUCT] The Self-Destruct timer drops to %d!\n" (x - 1)
        | _ ->
            for action_count = active_character.max_actions downto 1 do
              let action_successful = ref true in
              let first_try = ref true in
              while !first_try || not !action_successful do
                Printf.printf "Actions: %d/%d\n" action_count
                  active_character.max_actions ;
                match
                  get_int_selection "Choose an action:"
                    (List.map string_of_action actions)
                    false
                with
                | Some x -> (
                  match List.nth actions x with
                  | Move -> (
                    match character_move active_character [] true with
                    | None ->
                        print_endline "Canceled move" ;
                        action_successful := false
                    | Some r ->
                        Printf.printf "%s moved from %s to %s\n"
                          active_character.last_name
                          (locate_character active_character).name r.name ;
                        set_character_room active_character r )
                  | act ->
                      fatal rc_Error
                        ("Action " ^ string_of_action act ^ " is unsupported") )
                | None ->
                    ()
              done
            done )
      !game_state.characters ;
    broken := true
  done
