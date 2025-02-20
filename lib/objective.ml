open Item
open Utils
open Logging
open Map
open Encounter

(** Abstract type for named goals *)
type 'a goal =
  { goal_name: string  (** This goal's name *)
  ; kind: 'a  (** What kind of goal *)
  ; min_chars: int  (** Number of required characters *) }

(** Classes of objectives *)
type objective_kind =
  | BringItemToLocation of (item * string)
      (** Bring [item] to room designated by [string] *)
  | CrewAtLocationWithMinimumScrap of (string * int)
      (** Gather all crew at room designated by [string] with [int] scrap*)
  | DropCoolant of (int * string)
      (** Drop [int] coolant canisters in the room designated by [string] *)

(** Objective goals *)
type objective = objective_kind goal

(** Get string representation of objective *)
let string_of_objective (o : objective) : string =
  Printf.sprintf "%s - %s" o.goal_name
    ( match o.kind with
    | BringItemToLocation (i, r) ->
        Printf.sprintf "Bring one %s to %s" (string_of_item i) r
    | CrewAtLocationWithMinimumScrap (r, n) ->
        Printf.sprintf "All Crew members in %s" r
        ^
        if n = 0 then
          ""
        else
          Printf.sprintf
            " with at least %d Scrap in each Crew member's inventory." n
    | DropCoolant (n, s) ->
        Printf.sprintf "Drop %d COOLANT CANISTERS in %s" n s )

(** Classes of final missions *)
type final_mission_kind =
  | HurtAsh of (int * int * bool)
      (** Deal [int] damage to Ash, who tracks the Crew. If Ash meets a character 
      without a CoolantCanister to damage him, lose [int] morale. If [bool], 
      then use an incinerator on the xenomorph after Ash has been defeated to 
      win, otherwise the game ends when Ash is defeated
  *)
  | DropItemsAndAssemble of ((item * string) list * item list * string)
      (** Drop an [item] in the room designated by [string] for each member of the 
      first list for each crew member. The crew must cumulatively have the [item]s 
      corresponding to members of the second list. Then they must assemble at the 
      room designated by [string] to end the game
  *)
  | AlienCrewLocationsEncounter of
      (string * string list * (encounter -> bool) * string)
      (** The xenomorph must be in the room designated by [string]. At least one
      character must be in each of the rooms desginated by the members of
      [string list]. The game ends when a drawn encounter satisfies
      [encounter -> bool]. This condition is described by [string].
  *)
  | SelfDestructAssemble of (int * string * item * int)
      (** Start the self-destruct timer with [int] counters. A counter is removed
          at the start of the turn of the character who drew the final mission.
          The game is lost when all counters are gone. The game is won when
          characters assemble in the room designated by [string] holding an [item]
          and [int] scrap each
      *)
  | SelfDestructClear of int
      (** Start the self-destruct timer with [int] counters as in SelfDestructAssemble.
        The game is instead won by clearing the events which have been placed at
        every room.
    *)

type final_mission = final_mission_kind goal

let string_of_final_mission (fm : final_mission) (n_characters : int)
    (self_destruct_count : int) (ash_health : int)
    (rooms_with_events : room list) : string =
  Printf.sprintf "%s - %s" fm.goal_name
    ( match fm.kind with
    | HurtAsh (health, morale_drop, attack_xeno) ->
        Printf.sprintf
          "Deal %d damage to destroy Ash. Current health: %d. Ash is damaged \
           and knocked back by using %s %s on him.Approaching Ash without %s \
           %s will cause morale to drop.%s"
          health ash_health
          (article_of_item CoolantCanister)
          (string_of_item CoolantCanister)
          (article_of_item CoolantCanister)
          (string_of_item CoolantCanister)
          ( if attack_xeno then
              Printf.sprintf
                " Use %s %s on the xenomorph after Ash has been defeated to \
                 win."
                (article_of_item Incinerator)
                (string_of_item Incinerator)
            else
              " Defeat Ash to win." )
    | DropItemsAndAssemble (items_locations, crew_items, room) ->
        let items, locations = List.split items_locations in
        Printf.sprintf
          "Drop %s. The crew's inventory must contain: %s. Assemble all crew \
           members in %s to win."
          (String.concat ", "
             (List.map
                (fun (i, r) ->
                  Printf.sprintf "%d %s in %s" n_characters
                    (string_of_item ~plural:true i)
                    r )
                items_locations ) )
          (String.concat ", " (List.map string_of_item items))
          room
    | AlienCrewLocationsEncounter
        (* TODO : add support for extra special ability *)
        (xeno_room, character_rooms, _, encounter_condition) ->
        Printf.sprintf
          "The xenomorph must be in or adjacent to %s. There must be at least \
           one crew member in each of %s. Reveal one %s encounter to win."
          xeno_room
          (String.concat ", " character_rooms)
          encounter_condition
    | SelfDestructAssemble (timer, room, item, scrap) ->
        Printf.sprintf
          "The Nostromo has been set to self-destruct after %d rounds. %d \
           rounds remain. Assemble all crew members in %s, each holding %s %s \
           and %d scrap."
          timer self_destruct_count room (article_of_item item)
          (string_of_item item) scrap
    | SelfDestructClear timer ->
        Printf.sprintf
          "The Nostromo has been set to self-destruct after %d rounds. %d \
           rounds remain. Visit all rooms to clear them and win. Remaining \
           rooms: %s"
          timer self_destruct_count
          (String.concat ", " (List.map (fun r -> r.name) rooms_with_events)) )

(** List of objectives to pull from *)
let objectives_stack : objective list =
  [ { goal_name= "PREP SUITS"
    ; kind= DropCoolant (2, "SUIT STORAGE")
    ; min_chars= 1 }
  ; { goal_name= "WE'LL TAKE OUR CHANCES IN THE SHUTTLE"
    ; kind= DropCoolant (2, "DOCKING BAY")
    ; min_chars= 1 }
  ; { goal_name= "CREW MEETING"
    ; kind= CrewAtLocationWithMinimumScrap ("GALLEY", 1)
    ; min_chars= 1 }
  ; { goal_name= "WHAT'S THE DAMAGE?"
    ; kind= CrewAtLocationWithMinimumScrap ("WORKSHOP", 0)
    ; min_chars= 1 }
  ; { goal_name= "DRIVE 'EM INTO THE AIRLOCK"
    ; kind= BringItemToLocation (Incinerator, "AIRLOCK")
    ; min_chars= 1 }
  ; { goal_name= "WHERE IS IT?"
    ; kind= BringItemToLocation (Flashlight, "MED BAY")
    ; min_chars= 1 }
  ; { goal_name= "SHOULDN'T HAVE LANDED ON THIS BALL"
    ; kind= BringItemToLocation (GrappleGun, "GARAGE")
    ; min_chars= 1 }
  ; { goal_name= "GIVE IT A LITTLE INCENTIVE"
    ; kind= BringItemToLocation (ElectricProd, "GALLEY")
    ; min_chars= 1 }
  ; { goal_name= "ENCOUNTER THE NEST"
    ; kind= BringItemToLocation (Incinerator, "NEST")
    ; min_chars= 1 }
  ; { goal_name= "CHECK THE HYPERSLEEP CHAMBER"
    ; kind= BringItemToLocation (MotionTracker, "HYPERSLEEP")
    ; min_chars= 1 } ]

let final_mission_stack : final_mission list =
  [ { goal_name= "YOU HAVE MY SYMPATHIES"
    ; kind= HurtAsh (3, 3, true)
    ; min_chars= 1 }
  ; { goal_name= "ESCAPE ON THE NARCISSUS"
    ; kind=
        DropItemsAndAssemble
          ( [(CoolantCanister, "DOCKING BAY")]
          , [CatCarrier; Incinerator]
          , "DOCKING BAY" )
    ; min_chars= 1 }
  ; { goal_name= "BLOW IT OUT INTO SPACE"
    ; kind=
        AlienCrewLocationsEncounter
          ("DOCKING BAY", ["AIRLOCK"; "BRIDGE"], encounter_is_alien, "xenomorph")
    ; min_chars= 2 }
  ; { goal_name= "WE'RE GOING TO BLOW UP THE SHIP"
    ; kind= SelfDestructAssemble (4, "AIRLOCK", CoolantCanister, 1)
    ; min_chars= 1 }
  ; { goal_name= "CUT OFF EVERY BULKHEAD AND VENT"
    ; kind= SelfDestructClear 4
    ; min_chars= 2 } ]

(** Get a random list of [n] objectives*)
let get_objectives (n : int) : objective list =
  if n >= List.length objectives_stack then
    fatal rc_Error "Tried to get more objectives than are available"
  else
    List.take n (shuffle objectives_stack)

(** Get a random final mission *)
let get_final_mission () : final_mission = select_random final_mission_stack
