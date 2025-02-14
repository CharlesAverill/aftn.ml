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
  | TradeItem
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
  | TradeItem ->
      "Trade item"
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
  ; TradeItem
  ; EndTurn
  ; ViewRoom
  ; ViewCharacterLocations
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
  ; ViewCharacterLocations
  ; SeeObjectives
  ; DrawMap
  ; Exit ]

let action_select_chars =
  ['m'; 'p'; 'd'; 'a'; 'i'; 'k'; 'c'; 'u'; 't'; 'e'; 'v'; 'l'; 'o'; 'M'; 'x']

let single_player_action_select_chars =
  ['m'; 'p'; 'd'; 'a'; 'i'; 'k'; 'c'; 'u'; 'e'; 'v'; 'l'; 'o'; 'M'; 'x']

(** Prompt player for room to move to, return [false] if action canceled *)
let character_move (active_character : character) (allowed_moves : room list)
    (allow_back : bool) : bool =
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
  update_objectives () ;
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
      false
  | Some i -> (
    match find_room !game_state.map (List.nth allowed_moves i) with
    | None ->
        _log Log_Error
          "find_room in character_move returned None when it shouldn't have" ;
        false
    | Some r ->
        Printf.printf "%s moved from %s to %s\n" active_character.last_name
          (locate_character active_character).name r.name ;
        set_character_room active_character r ;
        true )

(** Prompt player for item/scrap to pick up. If an [item] is selected, [true]
    is returned to indicate the turn is over, otherwise [false].
*)
let pickup (active_character : character) : bool =
  let current_room = locate_character active_character in
  if
    !game_state.num_scrap current_room = 0
    && !game_state.room_items current_room = []
  then (
    print_endline "This room has no scrap or items to pick up" ;
    false
  ) else if List.length (!game_state.character_items active_character) >= 3 then (
    print_endline (active_character.last_name ^ " cannot carry another item") ;
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
                print_endline "Invalid scrap choice"
            | Some x ->
                if x > !game_state.num_scrap current_room then
                  print_endline "Invalid scrap choice"
                else
                  delta_scrap := x
        done ;
        Printf.printf "%s picked up %d scrap\n" active_character.last_name
          !delta_scrap ;
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

(** Prompt the user to drop an [item] in the [room] they are located in *)
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
        print_endline "Canceled drop" ;
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

(** Show the active character's inventory *)
let view_inventory ?(print_name : bool = false) (active_character : character) :
    unit =
  Printf.printf "=====%sINVENTORY=====\n"
    ( if print_name then
        active_character.last_name ^ " "
      else
        "" ) ;
  Printf.printf "Scrap: %d\n" (!game_state.character_scraps active_character) ;
  print_endline "Items:" ;
  List.iter
    (fun i -> print_endline ("\t" ^ string_of_item i))
    (!game_state.character_items active_character)

(** Show the team's morale and inventories *)
let view_team () : unit =
  print_endline "=====TEAM=====" ;
  Printf.printf "Morale: %d\n" !game_state.morale ;
  List.iter (view_inventory ~print_name:true) !game_state.characters

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
    print_endline
      (active_character.last_name ^ " doesn't have any scrap to craft with") ;
    false
  ) else if List.length (!game_state.character_items active_character) >= 3 then (
    print_endline (active_character.last_name ^ " cannot carry another item") ;
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
    print_endline "There is nobody to trade with" ;
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
            let end_turn = ref false in
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
                  | Move ->
                      if not (character_move active_character [] true) then (
                        print_endline "Canceled move" ;
                        action_successful := false
                      ) else
                        action_successful := true
                  | PickUp -> (
                    match pickup active_character with
                    | false ->
                        print_endline "Canceled pick up" ;
                        action_successful := false
                    | true ->
                        () )
                  | Drop ->
                      action_successful := drop active_character
                  | Ability ->
                      fatal rc_Error
                        ( "Action " ^ string_of_action Ability
                        ^ " is unsupported" )
                  | ViewInventory ->
                      view_inventory active_character ;
                      action_successful := false
                  | ViewTeam ->
                      view_team () ;
                      action_successful := false
                  | Craft ->
                      action_successful := craft active_character
                  | UseItem ->
                      fatal rc_Error
                        ( "Action " ^ string_of_action UseItem
                        ^ " is unsupported" )
                  | TradeItem ->
                      if not (trade_item active_character) then (
                        print_endline "Trade canceled" ;
                        action_successful := false
                      )
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
                  | ViewCharacterLocations ->
                      fatal rc_Error
                        ( "Action "
                        ^ string_of_action ViewCharacterLocations
                        ^ " is unsupported" )
                  | SeeObjectives ->
                      fatal rc_Error
                        ( "Action "
                        ^ string_of_action SeeObjectives
                        ^ " is unsupported" )
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
                        action_successful := false )
                | None ->
                    ()
              done
            done )
      !game_state.characters ;
    game_state := {!game_state with round_count= !game_state.round_count + 1}
  done
