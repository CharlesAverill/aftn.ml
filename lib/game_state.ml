(** Tracking the state of the currently-running game *)

open Map
open Character
open Encounter
open Objective
open Utils

(** Running game state *)
type state =
  { map: map  (** Game map *)
  ; characters: character list  (** Characters in play *)
  ; active_character: character option  (** Currently-playing character *)
  ; character_rooms: room list  (** Location of characters in play *)
  ; xeno_room: room  (** Location of xenomorph *)
  ; ash_room: room option  (** Location of Ash, if in play *)
  ; ash_health: int
        (** Ash's health for final mission "You Have My Sympathies" *)
  ; ash_killed: bool (*** Whether Ash has been killed *)
  ; jonesy_caught: bool  (** Whether Jonesy has been caught *)
  ; round_count: int  (** How many rounds have passed *)
  ; turn_idx: int
        (** Index into character list that designates which character's turn it is *)
  ; self_destruct_count: int option  (** How many turns until self destruct *)
  ; character_scraps: int list  (** Scrap per character *)
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
    ; characters= []
    ; active_character= None
    ; character_rooms= []
    ; xeno_room= blank_map.xeno_start_room
    ; ash_room= Some blank_map.ash_start_room
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
    ; character_scraps= []
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
  snd
    (List.find
       (fun (c', r) -> c'.last_name = c.last_name)
       (List.combine !game_state.characters !game_state.character_rooms) )

(** Add a [character] to a room *)
let add_character (c : character) (r : room) : unit =
  game_state :=
    { !game_state with
      characters= !game_state.characters @ [c]
    ; character_rooms= !game_state.character_rooms @ [r]
    ; character_scraps= !game_state.character_scraps @ [0] }

(** Get the room a [character] is in, or [None] if no such [character] exists *)
let get_character_room (c : character) : room option =
  match List.find_index (ch_eq c) !game_state.characters with
  | None ->
      None
  | Some idx ->
      Some (List.nth !game_state.character_rooms idx)

(** Set the position of a [character] *)
let set_character_room (c : character) (r : room) : unit =
  match List.find_index (ch_eq c) !game_state.characters with
  | None ->
      ()
  | Some idx ->
      game_state :=
        { !game_state with
          character_rooms= replacei !game_state.character_rooms idx r }

(** Determine the number of scrap a [character] has *)
let get_character_scrap (c : character) : int =
  match List.find_index (ch_eq c) !game_state.characters with
  | None ->
      0
  | Some idx ->
      List.nth !game_state.character_scraps idx

(** Set the number of scrap a [character] has *)
let set_character_scrap (c : character) (s : int) : unit =
  match List.find_index (ch_eq c) !game_state.characters with
  | None ->
      ()
  | Some idx ->
      game_state :=
        { !game_state with
          character_scraps= replacei !game_state.character_scraps idx s }

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
