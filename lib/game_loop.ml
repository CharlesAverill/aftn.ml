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
open Objective
open Logging
open Item
open Events
open Encounter
open Utils

let win_game () =
  Printf.printf "%s\n" "!!!!![GAME COMPLETE] - Congratulations!" ;
  Printf.printf "%s\n" "Your score will be recorded" ;
  reset_terminal () ;
  (* TODO : implement *)
  exit 0

let lose_game ?(game_over : bool = true) (message : string) =
  reset_terminal () ;
  if game_over then
    Printf.printf "!!!!![GAME OVER] - %s!!!!!\n" message
  else
    () ;
  exit 0

(** Set up the game state given user preferences *)
let setup_game (n_characters : int) (use_ash : bool) : unit =
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
        ( if use_ash then
            List.find_index
              (fun x -> x.name = !game_state.map.ash_start_room.name)
              !game_state.map.rooms
          else
            None
          (* Place Ash*) ) } ;
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
      if !has_coolant then add_room_item r CoolantCanister )
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
        Printf.printf "%s\n"
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
          lose_game ~game_over:false ""
      | Some n ->
          add_character (List.nth !char_options n)
            !game_state.map.player_start_room ;
          selected := !selected + 1 ;
          char_options :=
            List.filter
              (fun c -> ch_neq c (List.nth !char_options n))
              !char_options
    done ;
    Printf.printf "%s\n" "Characters:" ;
    List.iteri
      (fun i c -> Printf.printf "\t%d) %s\n" (i + 1) c.last_name)
      !game_state.characters ;
    game_state :=
      { !game_state with
        objectives=
          get_objectives (n_characters + 1) (* Get random objectives *) } ;
    shuffle_encounters ()

let complete_objective o =
  Printf.printf "[OBJECTIVE] - Completed objective \"%s\"!\n" o.goal_name ;
  clear_objective o

let setup_final_mission (active_character : character) : unit =
  match !game_state.final_mission with
  | None ->
      unreachable ()
  | Some final_mission ->
      ( Printf.printf "%s\n" "=====FINAL MISSION - YOU HAVE A NEW OBJECTIVE=====" ;
        match final_mission.kind with
        | HurtAsh (health, _, _) ->
            (* Fill equipment storage with coolant *)
            for _ = 1 to List.length !game_state.characters + 2 do
              add_room_item
                (find_room !game_state.map "EQUIPMENT STORAGE")
                CoolantCanister
            done ;
            Printf.printf "%s\n"
              "Coolant has been discovered in EQUIPMENT STORAGE!" ;
            (* Spawn Ash *)
            set_ash_room (find_room !game_state.map "MU-TH-UR") ;
            game_state :=
              {!game_state with ash_health= health; ash_killed= false} ;
            Printf.printf "%s\n" "Ash has been located in MU-TH-UR!"
        | DropItemsAndAssemble _ ->
            (* Fill equipment storage with coolant *)
            for _ = 1 to List.length !game_state.characters + 2 do
              add_room_item
                (find_room !game_state.map "EQUIPMENT STORAGE")
                CoolantCanister
            done ;
            Printf.printf "%s\n"
              "Coolant has been discovered in EQUIPMENT STORAGE!"
        | AlienCrewLocationsEncounter _ ->
            replace_all_encounters () ; shuffle_encounters ()
        | SelfDestructAssemble (counter, _, _, _) ->
            (* Fill equipment storage with coolant *)
            for _ = 1 to List.length !game_state.characters + 2 do
              add_room_item
                (find_room !game_state.map "EQUIPMENT STORAGE")
                CoolantCanister
            done ;
            Printf.printf "%s\n"
              "Coolant has been discovered in EQUIPMENT STORAGE!" ;
            Printf.printf "%s has the self-destruct counter!\n"
              active_character.last_name ;
            game_state :=
              { !game_state with
                self_destruct_count= Some counter
              ; self_destruct_character= Some active_character }
        | SelfDestructClear counter ->
            List.iter
              (fun r -> set_room_has_event r true)
              (List.filter (fun r -> r.is_corridor) !game_state.map.rooms) ;
            Printf.printf "%s has the self-destruct counter!\n"
              active_character.last_name ;
            game_state :=
              { !game_state with
                self_destruct_count= Some counter
              ; self_destruct_character= Some active_character } ) ;
      Printf.printf "%s\n"
        (string_of_final_mission final_mission
           (List.length !game_state.characters)
           (match !game_state.self_destruct_count with None -> 0 | Some x -> x)
           !game_state.ash_health
           (List.filter !game_state.has_event !game_state.map.rooms) )

(** Check if final mission criteria have been met, win game if so *)
let update_final_mission () : unit =
  if !game_state.final_mission != None then
    if
      match !game_state.final_mission with
      | None ->
          unreachable ()
      | Some fm -> (
        match fm.kind with
        | HurtAsh _ ->
            (* This check is performed in use_item *)
            false
        | DropItemsAndAssemble (items_locations, required_crew_items, room) ->
            let room = find_room !game_state.map room in
            let items_locations_correct =
              List.fold_left
                (fun a (i, r) ->
                  let room = find_room !game_state.map r in
                  a
                  && List.length
                       (List.filter
                          (fun x -> x = i)
                          (!game_state.room_items room) )
                     >= List.length !game_state.characters )
                true items_locations
            in
            let crew_items =
              List.fold_left
                (fun a c -> a @ !game_state.character_items c)
                [] !game_state.characters
            in
            let crew_items_correct =
              List.for_all
                (fun i -> List.exists (fun x -> x = i) crew_items)
                required_crew_items
            in
            items_locations_correct && crew_items_correct
            && List.for_all
                 (fun c -> locate_character c = room)
                 !game_state.characters
        | AlienCrewLocationsEncounter _ ->
            (* This check is performed in trigger_encounter *)
            false
        | SelfDestructAssemble (_, room, item, n_scrap) ->
            let room = find_room !game_state.map room in
            List.for_all
              (fun c ->
                locate_character c = room
                && character_has_item item c
                && !game_state.character_scraps c >= n_scrap )
              !game_state.characters
        | SelfDestructClear _ ->
            List.for_all
              (fun r -> not (!game_state.has_event r))
              !game_state.map.rooms )
    then
      win_game ()

(** Check uncleared objectives and clear them if conditions are met *)
let update_objectives (active_character : character) : unit =
  List.iter
    (fun o ->
      match o.kind with
      | BringItemToLocation (i, room) ->
          let room = find_room !game_state.map room in
          (* Clear if applicable *)
          let _ =
            List.fold_left
              (fun cleared c ->
                if
                  (not cleared)
                  && locate_character c = room
                  && character_has_item i c
                then (
                  complete_objective o ; true
                ) else
                  cleared )
              false !game_state.characters
          in
          ()
      | CrewAtLocationWithMinimumScrap (room, n_scrap) ->
          let room = find_room !game_state.map room in
          if
            List.fold_left
              (fun a c ->
                a
                && !game_state.character_scraps c >= n_scrap
                && locate_character c = room )
              true !game_state.characters
          then
            complete_objective o
      | DropCoolant (n_coolant, room) ->
          let room = find_room !game_state.map room in
          if
            List.length
              (List.filter
                 (fun i -> i = CoolantCanister)
                 (!game_state.room_items room) )
            >= n_coolant
          then
            complete_objective o )
    !game_state.objectives ;
  if !game_state.final_mission = None then (
    if !game_state.objectives = [] then (
      Printf.printf "%s\n" "=====COMPLETED ALL OBJECTIVES=====" ;
      let x = ref (select_random final_mission_stack) in
      while List.length !game_state.characters < !x.min_chars do
        x := select_random final_mission_stack
      done ;
      game_state := {!game_state with final_mission= Some !x} ;
      setup_final_mission active_character
    )
  ) else
    update_final_mission ()

let see_objectives () : unit =
  if List.length !game_state.objectives > 0 then (
    Printf.printf "%s\n" "=====OBJECTIVES=====" ;
    List.iter
      (fun o -> Printf.printf "%s\n" (string_of_objective o))
      !game_state.objectives
  ) ;
  if List.length !game_state.cleared_objectives > 0 then (
    Printf.printf "%s\n" "=====CLEARED OBJECTIVES=====" ;
    List.iter
      (fun o -> Printf.printf "%s\n" (string_of_objective o))
      !game_state.cleared_objectives
  ) ;
  if !game_state.final_mission != None then (
    Printf.printf "%s\n" "=====FINAL MISSION=====" ;
    match !game_state.final_mission with
    | None ->
        unreachable ()
    | Some fm ->
        Printf.printf "%s\n"
          (string_of_final_mission fm
             (List.length !game_state.characters)
             ( match !game_state.self_destruct_count with
             | None ->
                 0
             | Some x ->
                 x )
             !game_state.ash_health
             (List.filter !game_state.has_event !game_state.map.rooms) )
  )

(** Reduce morale by [n] and check for morale-loss-reduction items and game over *)
let reduce_morale (n : int) (saw_xeno : bool) : unit =
  let flashlight_chars =
    List.filter (character_has_item Flashlight) !game_state.characters
  in
  let prod_chars =
    List.filter (character_has_item ElectricProd) !game_state.characters
  in
  let discount =
    if
      saw_xeno
      && (not (List.is_empty flashlight_chars))
      && not (List.is_empty prod_chars)
    then (
      let options =
        List.map (fun c -> (c, Flashlight, 1)) flashlight_chars
        @ List.map (fun c -> (c, ElectricProd, 2)) prod_chars
      in
      match
        get_int_selection ~back_string:"No"
          "These characters have a FLASHLIGHT or an ELECTRIC PROD, would you \
           like to use one to reduce morale lost by 1 or 2, respectively?"
          (List.map
             (fun (c, i, m) ->
               Printf.sprintf "%s has %s %s - reduce lost morale by %d"
                 c.last_name (article_of_item i) (string_of_item i) m )
             options )
          true
      with
      | None ->
          0
      | Some idx ->
          let use_character, i, discount = List.nth options idx in
          remove_character_item use_character i ;
          discount
    ) else
      0
  in
  game_state := {!game_state with morale= !game_state.morale - (n - discount)} ;
  if !game_state.morale <= 0 then lose_game "Morale dropped to 0" ;
  Printf.printf "=====Morale drops by %d, morale is now %d=====\n"
    (n - discount) !game_state.morale

(** Prompt player for room to move to, return [None] if action canceled *)
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
    @ match ladder with None -> [] | Some r -> [r]
  in
  match
    get_int_selection "Destinations:"
      (List.map
         (fun r ->
           r ^ match ladder with Some x when x = r -> " (ladder)" | _ -> "" )
         (List.map
            (fun s ->
              if int_of_string_opt s = None then
                s
              else
                "Corridor " ^ s )
            allowed_moves ) )
      allow_back
  with
  | None ->
      None
  | Some i ->
      let r = find_room !game_state.map (List.nth allowed_moves i) in
      Printf.printf "%s moved from %s to %s\n" active_character.last_name
        (locate_character active_character).name r.name ;
      set_character_room active_character r ;
      update_objectives active_character ;
      Some r

(** Force a character to move 3 spaces away *)
let flee (active_character : character) : unit =
  Printf.printf "!!!!!%s must flee 3 spaces!!!!!\n" active_character.last_name ;
  let allowed_moves =
    find_rooms_by_distance !game_state.map (locate_character active_character) 3
  in
  ( match character_move active_character allowed_moves false with
  | None ->
      fatal rc_Error "Failed to select move while fleeing"
  | Some x ->
      set_character_room active_character x ) ;
  update_objectives active_character

(** Move the xenomorph closer to the closest team member *)
let xeno_move (num_spaces : int) (morale_drop : int) : bool =
  let shortest_path_to_nearest_char =
    match
      List.map
        (shortest_path !game_state.map (get_xeno_room ()))
        (List.map locate_character !game_state.characters)
    with
    | [] ->
        fatal rc_Error "list of shortest paths to each character is empty"
    | h :: t ->
        List.fold_left
          (fun a l ->
            if List.length l < List.length a then
              l
            else
              a )
          h t
  in
  set_xeno_room
    (List.nth shortest_path_to_nearest_char
       (min num_spaces (List.length shortest_path_to_nearest_char - 1)) ) ;
  let saw_xeno = ref false in
  List.iter
    (fun c ->
      if locate_character c = get_xeno_room () then (
        if not !saw_xeno then (
          saw_xeno := true ;
          Printf.printf "!!!!!The Xenomorph meets you in %s!!!!!\n"
            (get_xeno_room ()).name
        ) ;
        reduce_morale morale_drop true ;
        flee c
      ) )
    !game_state.characters ;
  !saw_xeno

let check_ash_health () : unit =
  match !game_state.final_mission with
  | Some fm -> (
    match fm.kind with
    | HurtAsh (_, _, hurt_xeno) when not !game_state.ash_killed ->
        if !game_state.ash_health = 0 then (
          game_state := {!game_state with ash_killed= true} ;
          if hurt_xeno then
            Printf.printf
              "[FINAL OBJECTIVE] - You've defeated Ash! Use %s %s on the \
               xenomorph to escape!\n"
              (article_of_item Incinerator)
              (string_of_item Incinerator)
          else
            win_game ()
        ) else
          Printf.printf "[FINAl OBJECTIVE] - Ash health = %d\n"
            !game_state.ash_health
    | _ ->
        () )
  | _ ->
      ()

let rec ash_move (num_spaces : int) : unit =
  match !game_state.ash_room with
  | Some ash_room when not !game_state.ash_killed ->
      let ash_room = List.nth !game_state.map.rooms ash_room in
      let hurt_ash_active =
        match !game_state.final_mission with
        | None ->
            false
        | Some fm -> (
          match fm.kind with HurtAsh _ -> true | _ -> false )
      in
      let shortest_path_to_nearest_char =
        match
          List.map
            (shortest_path !game_state.map ash_room)
            (List.map locate_character !game_state.characters)
        with
        | [] ->
            fatal rc_Error "list of shortest paths to each character is empty"
        | h :: t ->
            List.fold_left
              (fun a l ->
                if List.length l < List.length a then
                  l
                else
                  a )
              h t
      in
      let shortest_path_to_nearest_scrap_room =
        match
          List.map
            (shortest_path !game_state.map ash_room)
            (List.filter
               (fun r -> !game_state.num_scrap r > 0)
               !game_state.map.rooms )
        with
        | [] ->
            []
        | h :: t ->
            List.fold_left
              (fun a l ->
                if List.length l < List.length a then
                  l
                else
                  a )
              h t
      in
      let target_path =
        if hurt_ash_active then
          shortest_path_to_nearest_char
        else if
          List.length shortest_path_to_nearest_char
          < List.length shortest_path_to_nearest_scrap_room
        then
          shortest_path_to_nearest_char
        else
          shortest_path_to_nearest_scrap_room
      in
      let ash_room =
        List.nth target_path (min num_spaces (List.length target_path - 1))
      in
      set_ash_room ash_room ;
      set_room_scrap ash_room 0 ;
      let saw_ash = ref false in
      List.iter
        (fun c ->
          if locate_character c = ash_room then (
            if not !saw_ash then (
              saw_ash := true ;
              Printf.printf "!!!!!Ash meets you in %s!!!!!\n" ash_room.name
            ) ;
            if not hurt_ash_active then
              if !game_state.character_scraps c > 0 then (
                Printf.printf "%s lost %d scrap!\n" c.last_name 1 ;
                set_character_scrap c (!game_state.character_scraps c - 1)
              ) else (
                Printf.printf "%s has no scrap to lose!\n" c.last_name ;
                reduce_morale 1 false
              )
            else if character_has_item CoolantCanister c then (
              Printf.printf "%s uses %s %s to hurt Ash!\n" c.last_name
                (article_of_item CoolantCanister)
                (string_of_item CoolantCanister) ;
              remove_character_item c CoolantCanister ;
              game_state :=
                {!game_state with ash_health= !game_state.ash_health - 1} ;
              if !game_state.ash_health > 0 then
                (* Move Ash away *)
                let ash_locations =
                  find_rooms_within_distance !game_state.map ash_room 3
                in
                match
                  get_int_selection "Where to send Ash to?"
                    (List.map (fun r -> r.name) ash_locations)
                    false
                with
                | None ->
                    unreachable ()
                | Some idx ->
                    let ash_room' = List.nth ash_locations idx in
                    set_ash_room ash_room' ;
                    Printf.printf "Ash retreats to %s!\n" ash_room'.name ;
                    ash_move 0
            ) else (
              reduce_morale 3 false ; flee c
            ) ;
            check_ash_health ()
          ) )
        !game_state.characters
  | _ ->
      ()

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
  | TradeItem
  | EndTurn
  | ViewRoom
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
  | TradeItem ->
      "Trade item"
  | EndTurn ->
      "End turn early"
  | ViewRoom ->
      "View current room"
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
  ; TradeItem
  ; EndTurn
  ; ViewRoom
  ; SeeObjectives
  ; DrawMap
  ; Exit ]

let single_player_actions =
  [ Move
  ; PickUp
  ; Drop
  ; Ability
  ; ViewInventory
  ; ViewTeam
  ; Craft
  ; UseItem
  ; EndTurn
  ; ViewRoom
  ; SeeObjectives
  ; DrawMap
  ; Exit ]

let action_select_chars =
  ['m'; 'p'; 'd'; 'a'; 'i'; 'k'; 'c'; 'u'; 't'; 'e'; 'v'; 'o'; 'M'; 'x']

let single_player_action_select_chars =
  ['m'; 'p'; 'd'; 'a'; 'i'; 'k'; 'c'; 'u'; 'e'; 'v'; 'o'; 'M'; 'x']

(** Prompt player for item/scrap to pick up. If an [item] is selected, [true]
    is returned to indicate the turn is over, otherwise [false].
*)
let pickup (active_character : character) : bool =
  let current_room = locate_character active_character in
  if
    !game_state.num_scrap current_room = 0
    && !game_state.room_items current_room = []
  then (
    Printf.printf "%s\n" "This room has no scrap or items to pick up" ;
    false
  ) else if
      List.length
        (List.filter
           (fun i -> i != CoolantCanister)
           (!game_state.character_items active_character) )
      >= 3
    then (
    Printf.printf "%s\n"
      (active_character.last_name ^ " cannot carry another item") ;
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
        Printf.printf "Pick up how much scrap? (blank for all) " ;
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
                Printf.printf "%s\n" "Invalid scrap choice"
            | Some x ->
                if x > !game_state.num_scrap current_room then
                  Printf.printf "%s\n" "Invalid scrap choice"
                else
                  delta_scrap := x
        done ;
        Printf.printf "%s picked up %d scrap\n" active_character.last_name
          !delta_scrap ;
        set_character_scrap active_character
          (!game_state.character_scraps active_character + !delta_scrap) ;
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
            Printf.printf "%s\n"
              ( active_character.last_name ^ " already carries a "
              ^ string_of_item i ) ;
            false
          ) else (
            add_character_item active_character i ;
            true
          ) )

(** Prompt the user to drop an [item] in the [room] they are located in *)
let drop (active_character : character) : bool =
  let current_room = locate_character active_character in
  if !game_state.character_items active_character = [] then (
    Printf.printf "%s\n" (active_character.last_name ^ " has no items to drop") ;
    false
  ) else
    match
      get_int_selection "Which item to drop?"
        (List.map string_of_item (!game_state.character_items active_character))
        true
    with
    | None ->
        Printf.printf "%s\n" "Canceled drop" ;
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
            Printf.printf "%s\n"
              ( current_room.name ^ " already contains a "
              ^ string_of_item CoolantCanister ) ;
            false
          ) else (
            add_room_item current_room i ;
            true
          ) )

(** Show the active character's inventory *)
let view_inventory ?(print_name : bool = false) ?(print_location : bool = true)
    (active_character : character) : unit =
  Printf.printf "=====%sSTATUS=====\n"
    ( if print_name then
        active_character.last_name ^ " "
      else
        "" ) ;
  if print_location then
    Printf.printf "Location: %s\n" (locate_character active_character).name ;
  Printf.printf "Scrap: %d\n" (!game_state.character_scraps active_character) ;
  Printf.printf "%s\n" "Items:" ;
  List.iter
    (fun i -> Printf.printf "%s\n" ("\t" ^ string_of_item i))
    (!game_state.character_items active_character)

(** Show the team's morale and inventories *)
let view_team () : unit =
  Printf.printf "%s\n" "=======TEAM=======" ;
  Printf.printf "Morale: %d\n" !game_state.morale ;
  List.iter
    (view_inventory ~print_name:true ~print_location:true)
    !game_state.characters

(** Prompt the user to craft an item with scrap and add it to the active character's inventory *)
let craft (active_character : character) : bool =
  let price =
    if ch_eq active_character brett then
      fun x ->
    if cost_of_item x >= 2 then
      cost_of_item x - 1
    else
      cost_of_item x
    else
      cost_of_item
  in
  if !game_state.character_scraps active_character <= 0 then (
    Printf.printf "%s\n"
      (active_character.last_name ^ " doesn't have any scrap to craft with") ;
    false
  ) else if
      List.length
        (List.filter
           (fun i -> i != CoolantCanister)
           (!game_state.character_items active_character) )
      >= 3
    then (
    Printf.printf "%s\n"
      (active_character.last_name ^ " cannot carry another item") ;
    false
  ) else
    let craftable =
      List.filter
        (fun i -> price i <= !game_state.character_scraps active_character)
        craftable_items
    in
    match
      get_int_selection "Craftable items:"
        (List.map string_of_item craftable)
        true
    with
    | None ->
        false
    | Some idx ->
        let i = List.nth craftable idx in
        Printf.printf "%s crafted %s\n" active_character.last_name
          (string_of_item i) ;
        add_character_item active_character i ;
        set_character_scrap active_character
          (!game_state.character_scraps active_character - price i) ;
        true

(** Performs the trade action between two characters *)
let perform_trade (giver : character) (receiever : character) : bool =
  let givable_items =
    List.filter
      (fun i ->
        if
          i = CoolantCanister
          && List.exists
               (fun x -> x = CoolantCanister)
               (!game_state.character_items receiever)
        then
          false
        else
          true )
      (!game_state.character_items giver)
  in
  let num_scrap = !game_state.character_scraps giver in
  match
    get_int_selection
      (Printf.sprintf "What will %s give to %s?" giver.last_name
         receiever.last_name )
      ( ( if num_scrap > 0 then
            [Printf.sprintf "%d scrap" num_scrap]
          else
            [] )
      @ List.map string_of_item givable_items )
      true
  with
  | None ->
      false
  | Some idx when idx = 0 && num_scrap > 0 ->
      let to_give = ref (-1) in
      let go_back = ref false in
      while !to_give < 0 do
        Printf.printf "How much scrap to give? Max: %d ('b' to go back) "
          num_scrap ;
        let input = String.lowercase_ascii (String.trim (read_line ())) in
        match int_of_string_opt input with
        | None when input = "b" ->
            go_back := true ;
            to_give := 1
        | None ->
            ()
        | Some x when x > num_scrap ->
            ()
        | Some x ->
            to_give := min x num_scrap
      done ;
      if !go_back then
        false
      else (
        Printf.printf "%s gave %d scrap to %s\n" giver.last_name !to_give
          receiever.last_name ;
        set_character_scrap giver (!game_state.character_scraps giver - !to_give) ;
        set_character_scrap receiever
          (!game_state.character_scraps receiever + !to_give) ;
        true
      )
  | Some idx -> (
      let idx =
        if num_scrap > 0 then
          idx - 1
        else
          idx
      in
      match pop_character_item giver idx with
      | None ->
          _log Log_Error
            "pop_character_item in trade_item returned None when it shouldn't \
             have" ;
          false
      | Some i ->
          Printf.printf "%s gave %s %s to %s\n" giver.last_name
            (article_of_item i) (string_of_item i) receiever.last_name ;
          add_character_item receiever i ;
          true )

(** Prompt user to trade items with another character *)
let trade_item (active_character : character) : bool =
  let potential_receivers =
    List.filter
      (fun c ->
        (not (ch_eq active_character c))
        && !game_state.character_rooms c
           = !game_state.character_rooms active_character
        && ( !game_state.character_scraps active_character > 0
           || List.length (!game_state.character_items active_character) > 0
              && List.length (!game_state.character_items c) < 3
              &&
              if
                List.length
                  (List.filter
                     (fun i -> i = CoolantCanister)
                     (!game_state.character_items active_character) )
                = 1
              then
                not
                  (List.exists
                     (fun i -> i = CoolantCanister)
                     (!game_state.character_items c) )
              else
                true ) )
      !game_state.characters
  in
  let potential_givers =
    List.filter
      (fun c ->
        (not (ch_eq c active_character))
        && !game_state.character_rooms active_character
           = !game_state.character_rooms c
        && ( !game_state.character_scraps c > 0
           || List.length (!game_state.character_items c) > 0
              && List.length (!game_state.character_items active_character) < 3
              &&
              if
                List.length
                  (List.filter
                     (fun i -> i = CoolantCanister)
                     (!game_state.character_items c) )
                = 1
              then
                not
                  (List.exists
                     (fun i -> i = CoolantCanister)
                     (!game_state.character_items active_character) )
              else
                true ) )
      !game_state.characters
  in
  let all =
    List.fold_left
      (fun seen c ->
        if List.mem c seen then
          seen
        else
          c :: seen )
      []
      (potential_givers @ potential_receivers)
  in
  if List.length all = 0 then (
    Printf.printf "%s\n" "There is nobody to trade with" ;
    false
  ) else
    match
      get_int_selection "Which character would you like to trade with?"
        (List.map
           (fun c ->
             c.last_name ^ " - "
             ^ String.concat ", "
                 ( ( if List.exists (ch_eq c) potential_receivers then
                       ["Give"]
                     else
                       [] )
                 @
                 if List.exists (ch_eq c) potential_givers then
                   ["Take"]
                 else
                   [] ) )
           all )
        true
    with
    | None ->
        false
    | Some idx -> (
        let trade_char = List.nth all idx in
        let options =
          ( if List.exists (ch_eq trade_char) potential_receivers then
              ["Give"]
            else
              [] )
          @
          if List.exists (ch_eq trade_char) potential_givers then
            ["Take"]
          else
            []
        in
        match
          if List.length options = 1 then
            Some (List.nth options 0)
          else
            match
              get_int_selection "Which trade action would you like to take?"
                options true
            with
            | None ->
                None
            | Some x ->
                Some (List.nth options x)
        with
        | Some "Give" ->
            perform_trade active_character trade_char
        | Some "Take" ->
            perform_trade trade_char active_character
        | _ ->
            false )

let use (c : character) : bool =
  let usable_items =
    List.filter item_uses_action (!game_state.character_items c)
  in
  match usable_items with
  | [] ->
      Printf.printf "%s has no items to use.\n" c.last_name ;
      false
  | _ -> (
    match
      get_int_selection
        (Printf.sprintf "Which item will %s use?" c.last_name)
        (List.map string_of_item usable_items)
        true
    with
    | None ->
        false
    | Some idx -> (
      match List.nth usable_items idx with
      | MotionTracker -> (
        match
          find_rooms_within_distance !game_state.map (locate_character c) 2
        with
        | [] ->
            Printf.printf "%s\n" "There are no scannable rooms nearby." ;
            false
        | l -> (
          match
            get_int_selection "Which room to scan?"
              (List.map (fun r -> r.name) l)
              true
          with
          | None ->
              false
          | Some idx ->
              let room = List.nth l idx in
              remove_character_item c MotionTracker ;
              let _ = trigger_event c (Some room) in
              true ) )
      | GrappleGun -> (
          let shortest_path_to_xenomorph =
            shortest_path !game_state.map (get_xeno_room ())
              (locate_character c)
          in
          if
            not
              ( List.length shortest_path_to_xenomorph
              <= 3 + 1 (* +1 because contains source *) )
          then (
            Printf.printf "%s\n" "The xenomorph is not close enough!" ;
            false
          ) else
            let xeno_locations =
              find_rooms_within_distance !game_state.map (get_xeno_room ()) 3
            in
            match
              get_int_selection "Where to send the xenomorph to?"
                (List.map (fun r -> r.name) xeno_locations)
                false
            with
            | None ->
                unreachable ()
            | Some idx ->
                let loc = List.nth xeno_locations idx in
                remove_character_item c GrappleGun ;
                Printf.printf "The xenomorph retreats to %s!\n" loc.name ;
                set_xeno_room loc ;
                let _ = xeno_move 0 2 in
                true )
      | Incinerator ->
          let shortest_path_to_xenomorph =
            shortest_path !game_state.map (get_xeno_room ())
              (locate_character c)
          in
          if
            not
              ( List.length shortest_path_to_xenomorph
              <= 3 + 1 (* +1 because contains source *) )
          then (
            Printf.printf "%s\n" "The xenomorph is not close enough!" ;
            false
          ) else (
            remove_character_item c Incinerator ;
            Printf.printf "The xenomorph retreats to %s!\n"
              !game_state.map.xeno_start_room.name ;
            set_xeno_room !game_state.map.xeno_start_room ;
            (* Check HurtAsh final missions *)
            ( match !game_state.final_mission with
            | None ->
                ()
            | Some fm -> (
              match fm.kind with
              | HurtAsh _ ->
                  if !game_state.ash_killed then win_game ()
              | _ ->
                  () ) ) ;
            true
          )
      | i ->
          fatal rc_Error ("Unreachable path via item " ^ string_of_item i) ) )

(** Print out room information *)
let view_room (active_character : character) : unit =
  let current_room = locate_character active_character in
  Printf.printf "=====%s=====\n"
    ( if current_room.is_corridor then
        "Corridor " ^ current_room.name
      else
        current_room.name ) ;
  Printf.printf "Has event: %b\n" (!game_state.has_event current_room) ;
  Printf.printf "Scrap: %d\n" (!game_state.num_scrap current_room) ;
  Printf.printf "%s\n"
    ( if !game_state.room_items current_room = [] then
        "No items"
      else
        "Items:" ) ;
  List.iteri
    (fun idx i -> Printf.printf "\t%d) %s\n" idx (string_of_item i))
    (!game_state.room_items current_room) ;
  Printf.printf "Connections: \n" ;
  List.iteri
    (fun i -> Printf.printf "\t%d) %s\n" (i + 1))
    (List.map
       (fun x ->
         match int_of_string_opt x with None -> x | Some _ -> "Corridor " ^ x )
       current_room.connections ) ;
  match current_room.ladder_connection with
  | None ->
      ()
  | Some s ->
      Printf.printf "Ladder connection: %s\n" s

(** Draw an encounter card and execute it *)
let trigger_encounter (active_character : character) : unit =
  let ash_in_play =
    !game_state.ash_room != None && not !game_state.ash_killed
  in
  match discard_encounter () with
  | None ->
      fatal rc_Error "No encounters to draw"
  | Some encounter -> (
      ( match !game_state.final_mission with
      | Some fm -> (
        match fm.kind with
        | AlienCrewLocationsEncounter
            (target_xeno_room, target_character_rooms, _, _) ->
            let target_xeno_room = find_room !game_state.map target_xeno_room in
            let acceptable_xeno_rooms =
              target_xeno_room.name :: target_xeno_room.connections
            in
            let xeno_in_right_place =
              List.exists
                (fun s -> s = (get_xeno_room ()).name)
                acceptable_xeno_rooms
            in
            let characters_in_right_places =
              List.for_all
                (fun s ->
                  List.exists
                    (fun (_, r) -> r.name = s)
                    (character_locations ()) )
                target_character_rooms
            in
            if xeno_in_right_place && characters_in_right_places then
              win_game ()
        | _ ->
            () )
      | _ ->
          () ) ;
      match encounter with
      | Quiet ->
          let target_room =
            select_random
              (List.filter (fun r -> not r.is_corridor) !game_state.map.rooms)
          in
          Printf.printf "[ENCOUNTER] - All is quiet in %s. The xenomorph lurks."
            target_room.name ;
          if ash_in_play then
            Printf.printf "%s\n" " Ash lurks."
          else
            Printf.printf "%s\n" "" ;
          (* Place scrap into target_room *)
          ( match Random.int_in_range ~min:1 ~max:11 with
          | x when x <= 8 ->
              set_room_scrap target_room (2 + !game_state.num_scrap target_room)
          | x when x <= 10 ->
              set_room_scrap target_room (3 + !game_state.num_scrap target_room)
          | _ ->
              set_room_scrap target_room (1 + !game_state.num_scrap target_room)
          ) ;
          (* Place events *)
          if
            match !game_state.final_mission with
            | Some fm -> (
              match fm.kind with SelfDestructClear _ -> false | _ -> true )
            | _ ->
                true
          then
            set_room_has_event target_room true ;
          let _ = xeno_move 1 2 in
          ash_move 1
      | Alien_LostTheSignal ->
          Printf.printf "[ENCOUNTER] - The xenomorph has returned to the %s."
            !game_state.map.xeno_start_room.name ;
          if ash_in_play then
            Printf.printf "%s\n" " Ash lurks."
          else
            Printf.printf "%s\n" "" ;
          set_xeno_room !game_state.map.xeno_start_room ;
          let _ = xeno_move 0 2 in
          ash_move 1 ;
          replace_alien_encounters ()
      | Alien_Stalk ->
          Printf.printf "[ENCOUNTER] - The xenomorph is stalking..." ;
          if ash_in_play then
            Printf.printf "%s\n" " Ash lurks."
          else
            Printf.printf "%s\n" "" ;
          let _ = xeno_move 3 3 in
          ash_move 1
      | Alien_Hunt ->
          Printf.printf "[ENCOUNTER] - The xenomorph is hunting!" ;
          if ash_in_play then
            Printf.printf "%s\n" " Ash lurks."
          else
            Printf.printf "%s\n" "" ;
          let _ = xeno_move 2 4 in
          ash_move 1
      | Order937_MeetMeInTheInfirmary ->
          Printf.printf "[ENCOUNTER] - %s travels to %s."
            active_character.last_name !game_state.map.ash_start_room.name ;
          if ash_in_play then
            Printf.printf "%s\n" " Ash gives chase!"
          else
            Printf.printf "%s\n" "" ;
          set_character_room active_character !game_state.map.ash_start_room ;
          update_objectives active_character ;
          ash_move 2
      | Order937_CrewExpendable ->
          Printf.printf "[ENCOUNTER] - %s loses all scrap!"
            active_character.last_name ;
          if ash_in_play then
            Printf.printf "%s\n" " Ash gives chase!"
          else
            Printf.printf "%s\n" "" ;
          replace_order937_encounters () ;
          set_character_scrap active_character 0 ;
          ash_move 2
      | Order937_CollatingData ->
          Printf.printf "[ENCOUNTER] - %s %s 1 scrap%s"
            (String.concat ", "
               (List.map (fun c -> c.last_name) !game_state.characters) )
            ( if List.length !game_state.characters > 1 then
                "lose"
              else
                "loses" )
            ( if List.length !game_state.characters > 1 then
                " each."
              else
                "." ) ;
          if ash_in_play then
            Printf.printf "%s\n" " Ash gives chase!"
          else
            Printf.printf "%s\n" "" ;
          List.iter
            (fun c -> set_character_scrap c (max 0 (get_character_scrap c - 1)))
            !game_state.characters ;
          ash_move 2 )

(** Prompt player to use a [character]'s ability. Return whether the ability was 
    used and whether it can be used again this turn *)
let use_ability (c : character) : bool * bool =
  match (c.ability, c.ability_description) with
  | Some ability, Some description ->
      if
        confirm
          (Some
             (Printf.sprintf "Use %s's ability? (\"%s\")" c.last_name
                description ) )
      then (
        let ao = ability !game_state.map c !game_state.characters in
        (* Move characters *)
        ( match ao.moved_character_index with
        | None ->
            ()
        | Some idx -> (
            let moved_char = List.nth !game_state.characters idx in
            let old_room = locate_character moved_char in
            match character_move moved_char [] true with
            | None ->
                ()
            | Some new_room ->
                Printf.printf "%s moved %s from %s to %s\n" c.last_name
                  moved_char.last_name old_room.name new_room.name ) ) ;
        (ao.used_action, ao.can_use_again_this_turn)
      ) else
        (false, false)
  | _, _ ->
      Printf.printf "%s's ability cannot be used right now.\n" c.last_name ;
      (false, false)

(** Main game loop *)
let game_loop () : unit =
  Printf.printf "%s\n"
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
  Printf.printf "%s\n" "Press enter to continue" ;
  let _ = read_line () in
  see_objectives () ;
  Printf.printf "%s\n" "Press enter to continue" ;
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
        | Some x
          when match !game_state.self_destruct_character with
               | None ->
                   false
               | Some c ->
                   ch_eq c active_character ->
            game_state := {!game_state with self_destruct_count= Some (x - 1)} ;
            Printf.printf
              "[SELF-DESTRUCT] The Self-Destruct timer drops to %d!\n" (x - 1) ;
            if x - 1 <= 0 then
              lose_game "The Nostromo self-destructed with the Crew still on it"
        | _ ->
            let end_turn = ref false in
            let do_encounter = ref true in
            let can_use_ability = ref true in
            for action_count = active_character.max_actions downto 1 do
              let action_successful = ref true in
              let first_try = ref true in
              while (not !end_turn) && (!first_try || not !action_successful) do
                first_try := false ;
                Printf.printf "%s's actions: %d/%d\n" active_character.last_name
                  action_count active_character.max_actions ;
                match
                  get_int_selection
                    ~keys:
                      ( if List.length !game_state.characters > 1 then
                          action_select_chars
                        else
                          single_player_action_select_chars )
                    "Choose an action:"
                    (List.map string_of_action
                       ( if List.length !game_state.characters > 1 then
                           actions
                         else
                           single_player_actions ) )
                    false
                with
                | Some x -> (
                  match
                    List.nth
                      ( if List.length !game_state.characters > 1 then
                          actions
                        else
                          single_player_actions )
                      x
                  with
                  | Move -> (
                    match character_move active_character [] true with
                    | None ->
                        Printf.printf "%s\n" "Canceled move" ;
                        action_successful := false
                    | Some _ ->
                        action_successful := true ;
                        (* Trigger events in new location *)
                        ( match trigger_event active_character None with
                        | XenoEvent ->
                            flee active_character ;
                            end_turn := true ;
                            do_encounter := false
                        | _ ->
                            () ) ;
                        (* Check if player moved into xeno room and trigger fleeing *)
                        if xeno_move 0 2 then (
                          end_turn := true ;
                          do_encounter := false
                        ) ;
                        ash_move 0 ;
                        update_objectives active_character ;
                        update_final_mission () )
                  | PickUp ->
                      action_successful := pickup active_character ;
                      if not !action_successful then
                        Printf.printf "%s\n" "Canceled pick up"
                  | Drop ->
                      action_successful := drop active_character
                  | Ability ->
                      let used_ability, can_use_again =
                        use_ability active_character
                      in
                      action_successful := used_ability ;
                      can_use_ability := !can_use_ability && can_use_again
                  | ViewInventory ->
                      view_inventory active_character ;
                      action_successful := false
                  | ViewTeam ->
                      view_team () ;
                      action_successful := false
                  | Craft ->
                      action_successful := craft active_character
                  | UseItem ->
                      action_successful := use active_character
                  | TradeItem ->
                      action_successful := trade_item active_character ;
                      if not !action_successful then
                        Printf.printf "%s\n" "Trade canceled"
                  | EndTurn ->
                      if
                        confirm
                          (Some
                             (Printf.sprintf
                                "Are you sure you want to end %s's turn early? \
                                 (y/n)"
                                active_character.last_name ) )
                      then
                        end_turn := true
                      else
                        action_successful := false
                  | ViewRoom ->
                      view_room active_character ;
                      action_successful := false
                  | SeeObjectives ->
                      see_objectives () ;
                      action_successful := false
                  | DrawMap ->
                      ( match !game_state.map.ascii_map with
                      | None ->
                          Printf.printf "%s\n" "No ASCII map to print"
                      | Some s ->
                          Printf.printf "%s\n" s ) ;
                      action_successful := false
                  | Exit ->
                      if
                        confirm
                          (Some "Are you sure you want to exit the game? (y/n) ")
                      then
                        lose_game ~game_over:false ""
                      else
                        action_successful := false )
                | None ->
                    unreachable ()
              done
            done ;
            if !do_encounter then trigger_encounter active_character )
      !game_state.characters ;
    _log Log_Debug ("Xenomorph location: " ^ (get_xeno_room ()).name) ;
    game_state := {!game_state with round_count= !game_state.round_count + 1}
  done
