(** Main game loop and player actions *)

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
open Item

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
    ; xeno_room=
        ( match
            List.find_index
              (fun x -> x.name = !game_state.map.xeno_start_room.name)
              !game_state.map.rooms
          with
        | None ->
            -1
        | Some x ->
            x )
        (* Place xenomorph *)
    ; ash_room=
        List.find_index
          (fun x -> x.name = !game_state.map.ash_start_room.name)
          !game_state.map.rooms
        (* Place Ash*) } ;
  List.iter
    (fun r ->
      let num_scrap = ref (!game_state.num_scrap r) in
      let has_event = ref (!game_state.has_event r) in
      let has_coolant = ref (has_coolant r) in
      if List.exists (fun x -> x = r) !game_state.map.scrap_rooms then
        num_scrap := 2 ;
      if List.exists (fun x -> x = r) !game_state.map.event_rooms then
        has_event := true ;
      if List.exists (fun x -> x = r) !game_state.map.coolant_rooms then
        has_coolant := true ;
      set_room_scrap r !num_scrap ;
      set_room_has_event r !has_event ;
      add_room_item r CoolantCanister )
    !game_state.map.rooms ;
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
    print_endline "Characters:" ;
    List.iteri
      (fun i c -> Printf.printf "\t%d) %s\n" (i + 1) c.last_name)
      !game_state.characters ;
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

let action_select_chars =
  ['m'; 'p'; 'd'; 'a'; 'i'; 'k'; 'c'; 'u'; 'g'; 'e'; 'v'; 'l'; 'o'; 'M'; 'x']

(** Prompt player for room to move to, then return it *)
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
        List.map
          (fun s ->
            if int_of_string_opt s = None then
              s
            else
              "Corridor " ^ s )
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

(** Prompt player for item/scrap to pick up. If an item is selected,
    (true, Some item) is returned. If scrap is selected, (true, None) is returned.
    If nothing is selected, or nothing is able to be picked up, (false, None) is 
    returned.
*)
let pickup (active_character : character) : bool =
  let current_room = locate_character active_character in
  if
    !game_state.num_scrap current_room = 0
    && !game_state.room_items current_room = []
  then (
    print_endline "This room has no scrap or items to pick up" ;
    false
  ) else
    let pick_up_options =
      ( if !game_state.num_scrap current_room > 0 then
          [string_of_int (!game_state.num_scrap current_room) ^ " scrap"]
        else
          [] )
      @ List.map string_of_item (!game_state.room_items current_room)
    in
    match get_int_selection "Pick up options:" pick_up_options true with
    | None ->
        false
    | Some i when !game_state.num_scrap current_room > 0 && i = 0 ->
        (* Pick up scrap *)
        Printf.printf "Pick up how much scrap? (Max: %d) "
          (!game_state.num_scrap current_room) ;
        let delta_scrap = ref (-1) in
        let go_back = ref false in
        while !delta_scrap < 0 do
          let input = read_line () in
          if
            String.lowercase_ascii input = "back"
            || String.lowercase_ascii input = "b"
          then (
            go_back := true ;
            delta_scrap := 0
          ) else if input = "" then
            delta_scrap := !game_state.num_scrap current_room
          else
            match int_of_string_opt input with
            | None ->
                print_endline "Invalid scrap choice"
            | Some x ->
                if x > !game_state.num_scrap current_room then
                  print_endline "Invalid scrap choice"
                else
                  delta_scrap := x
        done ;
        Printf.printf "Picked up %d scrap\n" !delta_scrap ;
        set_character_scrap active_character !delta_scrap ;
        set_room_scrap current_room
          (!game_state.num_scrap current_room - !delta_scrap) ;
        true
    | Some i -> (
      match
        pop_room_item current_room
          ( if !game_state.num_scrap current_room > 0 then
              i - 1
            else
              i )
      with
      | None ->
          _log Log_Warning "pop_room_item returned None when it shouldn't" ;
          false
      | Some i ->
          if
            i = CoolantCanister
            && List.exists
                 (fun x -> x = CoolantCanister)
                 (!game_state.character_items active_character)
          then (
            print_endline
              ( active_character.last_name ^ " already carries a "
              ^ string_of_item i ) ;
            false
          ) else (
            add_character_item active_character i ;
            true
          ) )

let view_inventory (active_character : character) : unit =
  print_endline "=====INVENTORY=====" ;
  Printf.printf "Scrap: %d\n" (!game_state.character_scraps active_character) ;
  print_endline "Items:" ;
  List.iter
    (fun i -> print_endline ("\t" ^ string_of_item i))
    (!game_state.character_items active_character)

let drop (active_character : character) : bool =
  let current_room = locate_character active_character in
  if !game_state.character_items active_character = [] then (
    print_endline (active_character.last_name ^ " has no items to drop") ;
    false
  ) else
    match
      get_int_selection "Which item to drop?"
        (List.map string_of_item (!game_state.character_items active_character))
        true
    with
    | None ->
        false
    | Some idx -> (
      match pop_character_item active_character idx with
      | None ->
          _log Log_Error
            "pop_character_item returned None when it shouldn't have" ;
          false
      | Some i ->
          if
            i = CoolantCanister
            && List.exists
                 (fun i -> i = CoolantCanister)
                 (!game_state.room_items current_room)
          then (
            print_endline
              ( current_room.name ^ " already contains a "
              ^ string_of_item CoolantCanister ) ;
            false
          ) else (
            add_room_item current_room i ;
            true
          ) )

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
        Printf.printf "-----Turn %d-----\n" (1 + !game_state.turn_idx) ;
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
                first_try := false ;
                Printf.printf "Actions: %d/%d\n" action_count
                  active_character.max_actions ;
                match
                  get_int_selection ~keys:action_select_chars
                    "Choose an action:"
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
                  | PickUp -> (
                    match pickup active_character with
                    | false ->
                        print_endline "Canceled pick up" ;
                        action_successful := false
                    | true ->
                        () )
                  | Drop -> (
                    match drop active_character with
                    | false ->
                        print_endline "Canceled drop" ;
                        action_successful := false
                    | true ->
                        () )
                  | ViewInventory ->
                      view_inventory active_character ;
                      action_successful := false
                  | DrawMap ->
                      ( match !game_state.map.ascii_map with
                      | None ->
                          print_endline "No ASCII map to print"
                      | Some s ->
                          print_endline s ) ;
                      action_successful := false
                  | Exit ->
                      if
                        confirm
                          (Some "Are you sure you want to exit the game? (y/n) ")
                      then
                        exit 0
                      else
                        action_successful := false
                  | act ->
                      fatal rc_Error
                        ("Action " ^ string_of_action act ^ " is unsupported") )
                | None ->
                    ()
              done
            done )
      !game_state.characters
  done
