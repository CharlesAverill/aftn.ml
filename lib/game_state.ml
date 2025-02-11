(** Tracking the state of the currently-running game *)

open Map
open Character
open Encounter
open Utils

(** Running game state *)
type state =
  { map: map  (** Game map *)
  ; characters: character list  (** Characters in play *)
  ; character_rooms: room list  (** Location of characters in play *)
  ; character_scraps: int list  (** Scrap per character *)
  ; encounters: encounter list  (** Un-drawn encounters *)
  ; discarded_encounters: encounter list  (** Discarded encounters *) }

(** Reference to the global game state *)
let game_state : state ref =
  ref
    { map= blank_map
    ; characters= []
    ; character_rooms= []
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
       (fun x -> fst x = c)
       (List.combine !game_state.characters !game_state.character_rooms) )

(** Add a [character] to a room *)
let add_character (c : character) (r : room) : unit =
  game_state :=
    { !game_state with
      characters= c :: !game_state.characters
    ; character_rooms= r :: !game_state.character_rooms
    ; character_scraps= 0 :: !game_state.character_scraps }

(** Get the room a [character] is in, or [None] if no such [character] exists *)
let get_character_room (c : character) : room option =
  match List.find_index (fun x -> x = c) !game_state.characters with
  | None ->
      None
  | Some idx ->
      Some (List.nth !game_state.character_rooms idx)

(** Set the position of a [character] *)
let set_character_room (c : character) (r : room) : unit =
  match List.find_index (fun x -> x = c) !game_state.characters with
  | None ->
      ()
  | Some idx ->
      game_state :=
        { !game_state with
          character_rooms= replacei !game_state.character_rooms idx r }

(** Determine the number of scrap a [character] has *)
let get_character_scrap (c : character) : int =
  match List.find_index (fun x -> x = c) !game_state.characters with
  | None ->
      0
  | Some idx ->
      List.nth !game_state.character_scraps idx

(** Set the number of scrap a [character] has *)
let set_character_scrap (c : character) (s : int) : unit =
  match List.find_index (fun x -> x = c) !game_state.characters with
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
