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
  ; num_scrap: room -> int  (** How much scrap is in this room *)
  ; has_event: room -> bool  (** Whether the room has an event *)
  ; room_items: room -> item list  (** Items in the room *)
  ; characters: character list  (** Characters in play *)
  ; active_character: character option  (** Currently-playing character *)
  ; character_rooms: character -> int
        (** Location of characters in play, index into [map.rooms] *)
  ; xeno_room: int  (** Location of xenomorph *)
  ; ash_room: int option  (** Location of Ash, if in play *)
  ; ash_health: int
        (** Ash's health for final mission "You Have My Sympathies" *)
  ; ash_killed: bool (*** Whether Ash has been killed *)
  ; jonesy_caught: bool  (** Whether Jonesy has been caught *)
  ; round_count: int  (** How many rounds have passed *)
  ; turn_idx: int
        (** Index into character list that designates which character's turn it is *)
  ; self_destruct_count: int option  (** How many turns until self destruct *)
  ; character_scraps: character -> int  (** Scrap per character *)
  ; character_items: character -> item list  (** Items per character *)
  ; encounters: encounter list  (** Un-drawn encounters *)
  ; discarded_encounters: encounter list  (** Discarded encounters *)
  ; morale: int  (** Team morale - game ends when it reaches 0 *)
  ; objectives: objective list
        (** Objectives to complete before the final mission is revealed *)
  ; on_final_mission: bool  (** Whether final mission is active *)
  ; final_mission: final_mission option
        (** What the game's final objective is *) }

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
    ; ash_room= Some (-1)
    ; ash_health= 0
    ; ash_killed= false
    ; jonesy_caught= false
    ; round_count= 1
    ; turn_idx= 0
    ; self_destruct_count= None
    ; morale= 0
    ; objectives= []
    ; on_final_mission= false
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

(** Get room a [character] is in *)
let locate_character (c : character) : room =
  List.nth !game_state.map.rooms (!game_state.character_rooms c)

(** Add a [character] to a room *)
let add_character (c : character) (r : room) : unit =
  match List.find_index (fun x -> x = r) !game_state.map.rooms with
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
  match List.find_index (fun x -> x = r) !game_state.map.rooms with
  | None ->
      ()
  | Some idx ->
      game_state :=
        { !game_state with
          character_rooms= ch_update !game_state.character_rooms c idx }

let set_room_scrap (r : room) (n : int) : unit =
  game_state := {!game_state with num_scrap= update !game_state.num_scrap r n}

let set_room_has_event (r : room) (b : bool) : unit =
  game_state := {!game_state with has_event= update !game_state.has_event r b}

let has_coolant (r : room) : bool =
  List.exists (fun x -> x = CoolantCanister) (!game_state.room_items r)

let add_room_item (r : room) (i : item) : unit =
  game_state :=
    { !game_state with
      room_items= update !game_state.room_items r (i :: !game_state.room_items r)
    }

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

let pop_room_item (r : room) (idx : int) : item option =
  match List.nth_opt (!game_state.room_items r) idx with
  | None ->
      None
  | Some x ->
      remove_room_item r x ; Some x

(** Determine the number of scrap a [character] has *)
let get_character_scrap : character -> int = !game_state.character_scraps

(** Set the number of scrap a [character] has *)
let set_character_scrap (c : character) (s : int) : unit =
  game_state :=
    { !game_state with
      character_scraps= ch_update !game_state.character_scraps c s }

let get_character_items : character -> item list = !game_state.character_items

let add_character_item (c : character) (i : item) : unit =
  game_state :=
    { !game_state with
      character_items=
        ch_update !game_state.character_items c
          (i :: !game_state.character_items c) }

let remove_character_item (c : character) (i : item) : unit =
  let item_idx =
    List.find_index (fun x -> x = i) (!game_state.character_items c)
  in
  let items' =
    List.filteri (fun x _ -> Some x != item_idx) (!game_state.character_items c)
  in
  game_state :=
    { !game_state with
      character_items= ch_update !game_state.character_items c items' }

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

let set_map (map_fn : string) : unit =
  game_state := {!game_state with map= parse_map_file map_fn}
