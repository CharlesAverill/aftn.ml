(** Tracking the state of the currently-running game *)

open Map
open Character
open Encounter
open Objective
open Utils
open Item
open Logging

(** Running game state *)
type state =
  { map: map  (** Game map *)
  ; num_scrap: room -> int  (** How much scrap is in this [room] *)
  ; has_event: room -> bool  (** Whether the [room] has an event *)
  ; room_items: room -> item list  (** Items in the [room] *)
  ; characters: character list  (** [characters]s in play *)
  ; active_character: character option  (** Currently-playing [character] *)
  ; character_rooms: character -> int
        (** Location of [character]s in play - index into [map.rooms] *)
  ; xeno_room: int  (** Location of xenomorph - index into [map.rooms] *)
  ; ash_room: int option
        (** Location of Ash, if in play - index into [map.rooms] *)
  ; ash_health: int
        (** Ash's health for final mission "You Have My Sympathies" *)
  ; ash_killed: bool (*** Whether Ash has been killed *)
  ; jonesy_caught: bool  (** Whether Jonesy has been caught *)
  ; round_count: int  (** How many rounds have passed *)
  ; turn_idx: int  (** How many turns have passed on this round *)
  ; self_destruct_count: int option  (** How many turns until self destruct *)
  ; self_destruct_character: character option
        (** On which character's turn does self-destruct counter decrease? *)
  ; character_scraps: character -> int  (** Scrap per [character] *)
  ; character_items: character -> item list  (** Items per [character] *)
  ; encounters: encounter list  (** Un-drawn [encounter]s *)
  ; discarded_encounters: encounter list  (** Discarded [encounter]s *)
  ; morale: int  (** Team morale *)
  ; objectives: objective list
        (** [objective]s to complete before the final mission is revealed *)
  ; cleared_objectives: objective list
        (** [objective]s that have been cleared *)
  ; final_mission: final_mission option
        (** What the game's final [objective] is *) }

(** Reference to the global game state *)
let game_state : state ref =
  ref
    { map= blank_map
    ; num_scrap= (fun _ -> 0)
    ; room_items= (fun _ -> [])
    ; has_event= (fun _ -> false)
    ; characters= []
    ; active_character= None
    ; character_rooms= (fun _ -> 0)
    ; xeno_room= -1
    ; ash_room= None
    ; ash_health= 0
    ; ash_killed= false
    ; jonesy_caught= false
    ; round_count= 1
    ; turn_idx= 0
    ; self_destruct_count= None
    ; self_destruct_character= None
    ; morale= 0
    ; objectives= []
    ; cleared_objectives= []
    ; final_mission= None
    ; character_scraps= (fun _ -> 0)
    ; character_items= (fun _ -> [])
    ; encounters=
        [ Quiet
        ; Quiet
        ; Quiet
        ; Quiet
        ; Quiet
        ; Quiet
        ; Quiet
        ; Quiet
        ; Quiet
        ; Quiet
        ; Quiet
        ; Alien_LostTheSignal
        ; Alien_Stalk
        ; Alien_Stalk
        ; Alien_Stalk
        ; Alien_Hunt
        ; Alien_Hunt
        ; Order937_MeetMeInTheInfirmary
        ; Order937_CrewExpendable
        ; Order937_CollatingData
        ; Order937_CollatingData ]
    ; discarded_encounters= [] }

(* Character state *)

(** Get [room] a [character] is in *)
let locate_character (c : character) : room =
  List.nth !game_state.map.rooms (!game_state.character_rooms c)

(** Add a [character] to a [room] *)
let add_character (c : character) (r : room) : unit =
  match List.find_index (fun x -> x.name = r.name) !game_state.map.rooms with
  | None ->
      fatal rc_Error ("Failed to add character to non-existent room " ^ r.name)
  | Some idx ->
      game_state :=
        { !game_state with
          characters= !game_state.characters @ [c]
        ; character_rooms= ch_update !game_state.character_rooms c idx
        ; character_scraps= ch_update !game_state.character_scraps c 0 }

(** Set the position of a [character] *)
let set_character_room (c : character) (r : room) : unit =
  match List.find_index (fun x -> x.name = r.name) !game_state.map.rooms with
  | None ->
      ()
  | Some idx ->
      game_state :=
        { !game_state with
          character_rooms= ch_update !game_state.character_rooms c idx }

(** Determine the number of scrap a [character] has *)
let get_character_scrap : character -> int = !game_state.character_scraps

(** Set the number of scrap a [character] has *)
let set_character_scrap (c : character) (s : int) : unit =
  game_state :=
    { !game_state with
      character_scraps= ch_update !game_state.character_scraps c s }

(** Get the items in a [character]'s inventory *)
let get_character_items : character -> item list = !game_state.character_items

(** Add an item to a [character]'s inventory *)
let add_character_item (c : character) (i : item) : unit =
  game_state :=
    { !game_state with
      character_items=
        ch_update !game_state.character_items c
          (i :: !game_state.character_items c) }

(** Remove an [item] from a [character]'s inventory *)
let remove_character_item (c : character) (i : item) : unit =
  match List.find_index (fun x -> x = i) (!game_state.character_items c) with
  | None ->
      _log Log_Error
        (Printf.sprintf "Failed to remove item %s from %s" (string_of_item i)
           c.last_name )
  | Some item_idx ->
      let items' =
        List.filteri (fun x _ -> x != item_idx) (!game_state.character_items c)
      in
      game_state :=
        { !game_state with
          character_items= ch_update !game_state.character_items c items' }

(** Remove an [item] from a [character] and return it *)
let pop_character_item (c : character) (idx : int) : item option =
  match List.nth_opt (!game_state.character_items c) idx with
  | None ->
      None
  | Some x ->
      remove_character_item c x ; Some x

(** Whether a [character] has a specific [item] in their inventory *)
let character_has_item (i : item) (c : character) : bool =
  List.exists (fun x -> x = i) (!game_state.character_items c)

(** List of characters and their locations *)
let character_locations () : (character * room) list =
  List.map (fun c -> (c, locate_character c)) !game_state.characters

(* Room state *)

(** Set the number of scrap in a [room] *)
let set_room_scrap (r : room) (n : int) : unit =
  game_state := {!game_state with num_scrap= update !game_state.num_scrap r n}

(** Set whether a room has an [event] *)
let set_room_has_event (r : room) (b : bool) : unit =
  game_state := {!game_state with has_event= update !game_state.has_event r b}

(** Check whether a [room] contains coolant *)
let has_coolant (r : room) : bool =
  List.exists (fun x -> x = CoolantCanister) (!game_state.room_items r)

(** Add an [item] to a [room] *)
let add_room_item (r : room) (i : item) : unit =
  game_state :=
    { !game_state with
      room_items= update !game_state.room_items r (i :: !game_state.room_items r)
    }

(** Remove an [item] from a [room] *)
let remove_room_item (r : room) (i : item) : unit =
  match List.find_index (fun x -> x = i) (!game_state.room_items r) with
  | None ->
      _log Log_Error
        (Printf.sprintf "Failed to remove item %s from room %s"
           (string_of_item i) r.name )
  | Some item_idx ->
      let items' =
        List.filteri (fun x _ -> x != item_idx) (!game_state.room_items r)
      in
      game_state :=
        {!game_state with room_items= update !game_state.room_items r items'}

(** Remove an [item] from a [room] and return it, if one exists *)
let pop_room_item (r : room) (idx : int) : item option =
  match List.nth_opt (!game_state.room_items r) idx with
  | None ->
      None
  | Some x ->
      remove_room_item r x ; Some x

(** Set the game's [map] *)
let set_map (map_fn : string) : unit =
  game_state := {!game_state with map= parse_map_file map_fn}

(* Encounter state *)

(** Shuffle the list of random [encounter]s *)
let shuffle_encounters () : unit =
  game_state := {!game_state with encounters= shuffle !game_state.encounters}

(** Remove the top [encounter] from the list of random [encounter]s *)
let discard_encounter () : encounter option =
  match !game_state.encounters with
  | [] ->
      None
  | h :: t ->
      game_state :=
        { !game_state with
          encounters= t
        ; discarded_encounters= h :: !game_state.discarded_encounters } ;
      Some h

(** Take the top [encounter] from the discarded [encounter]s list and replace it in the list of random [encounter]s *)
let replace_encounter () : unit =
  match !game_state.discarded_encounters with
  | [] ->
      ()
  | h :: t ->
      game_state :=
        { !game_state with
          encounters= h :: !game_state.encounters
        ; discarded_encounters= t }

let replace_all_encounters () : unit =
  while !game_state.discarded_encounters != [] do
    replace_encounter ()
  done

let replace_alien_encounters () : unit =
  game_state :=
    { !game_state with
      encounters=
        List.filter encounter_is_alien !game_state.discarded_encounters
        @ !game_state.encounters
    ; discarded_encounters=
        List.filter
          (fun x -> not (encounter_is_alien x))
          !game_state.discarded_encounters } ;
  shuffle_encounters ()

let replace_order937_encounters () : unit =
  game_state :=
    { !game_state with
      encounters=
        List.filter encounter_is_order937 !game_state.discarded_encounters
        @ !game_state.encounters
    ; discarded_encounters=
        List.filter
          (fun x -> not (encounter_is_order937 x))
          !game_state.discarded_encounters } ;
  shuffle_encounters ()

(* Misc state *)

let get_xeno_room () : room =
  List.nth !game_state.map.rooms !game_state.xeno_room

let set_xeno_room (r : room) : unit =
  game_state :=
    { !game_state with
      xeno_room=
        ( match
            List.find_index (fun x -> x.name = r.name) !game_state.map.rooms
          with
        | None ->
            fatal rc_Error "set_xeno_room couldn't find index of room"
        | Some r ->
            r ) }

let set_ash_room (r : room) : unit =
  game_state :=
    { !game_state with
      ash_room=
        ( match
            List.find_index (fun x -> x.name = r.name) !game_state.map.rooms
          with
        | None ->
            fatal rc_Error "get_xeno_room couldn't find index of room"
        | Some r ->
            Some r ) }

let set_ash_health (n : int) : unit =
  game_state := {!game_state with ash_health= n}

let set_ash_killed (b : bool) : unit =
  game_state := {!game_state with ash_killed= b}

(* Objectives state *)

let add_objective (o : objective) : unit =
  game_state := {!game_state with objectives= o :: !game_state.objectives}

let clear_objective (o : objective) : unit =
  game_state :=
    { !game_state with
      cleared_objectives= o :: !game_state.cleared_objectives
    ; objectives=
        snd
          (List.fold_left
             (fun (found, a) o' ->
               if found then
                 (true, a @ [o'])
               else if o = o' then
                 (true, a)
               else
                 (false, a @ [o']) )
             (false, []) !game_state.objectives ) }

let catch_jonesy () : unit = game_state := {!game_state with jonesy_caught= true}

let set_morale (n : int) : unit = game_state := {!game_state with morale= n}

let start_self_destruct (counter : int) (c : character) : unit =
  game_state :=
    { !game_state with
      self_destruct_count= Some counter
    ; self_destruct_character= Some c }

let set_final_mission (fm : final_mission) : unit =
  game_state := {!game_state with final_mission= Some fm}

let set_turn (idx : int) (c : character) : unit =
  game_state := {!game_state with turn_idx= idx; active_character= Some c}

let set_self_destruct_counter (n : int) : unit =
  game_state := {!game_state with self_destruct_count= Some n}

let incr_round () : unit =
  game_state := {!game_state with round_count= !game_state.round_count + 1}
