open Item
open Utils
open Logging
open Map
open Encounter

(** Abstract type for named goals *)
type 'a goal =
  { obj_name: string  (** This goal's name *)
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
  Printf.sprintf "%s - %s" o.obj_name
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
      (** Drop [item] in the room designated by [string] for each member of the 
      first list for each crew member. The crew must cumulatively have the [item]s 
      corresponding to members of the second list. Then they must assemble at the 
      room designated by [string] to end the game
  *)
  | AlienCrewLocationsEncounter of (string * string list * (encounter -> bool))
      (** The xenomorph must be in the room designated by [string]. At least one
      character must be in each of the rooms desginated by the members of
      [string list]. The game ends when a drawn encounter satisfies
      [encounter -> bool]
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

(** List of objectives to pull from *)
let objectives_stack : objective list =
  [ {obj_name= "PREP SUITS"; kind= DropCoolant (2, "SUIT STORAGE"); min_chars= 1}
  ; { obj_name= "WE'LL TAKE OUR CHANCES IN THE SHUTTLE"
    ; kind= DropCoolant (2, "DOCKING BAY")
    ; min_chars= 1 }
  ; { obj_name= "CREW MEETING"
    ; kind= CrewAtLocationWithMinimumScrap ("GALLEY", 1)
    ; min_chars= 1 }
  ; { obj_name= "WHAT'S THE DAMAGE?"
    ; kind= CrewAtLocationWithMinimumScrap ("WORKSHOP", 0)
    ; min_chars= 1 }
  ; { obj_name= "DRIVE 'EM INTO THE AIRLOCK"
    ; kind= BringItemToLocation (Incinerator, "AIRLOCK")
    ; min_chars= 1 }
  ; { obj_name= "WHERE IS IT?"
    ; kind= BringItemToLocation (Flashlight, "MED BAY")
    ; min_chars= 1 }
  ; { obj_name= "SHOULDN'T HAVE LANDED ON THIS BALL"
    ; kind= BringItemToLocation (GrappleGun, "GARAGE")
    ; min_chars= 1 }
  ; { obj_name= "GIVE IT A LITTLE INCENTIVE"
    ; kind= BringItemToLocation (ElectricProd, "GALLEY")
    ; min_chars= 1 }
  ; { obj_name= "ENCOUNTER THE NEST"
    ; kind= BringItemToLocation (Incinerator, "NEST")
    ; min_chars= 1 }
  ; { obj_name= "CHECK THE HYPERSLEEP CHAMBER"
    ; kind= BringItemToLocation (MotionTracker, "HYPERSLEEP")
    ; min_chars= 1 } ]

let final_mission_stack : final_mission list =
  [ { obj_name= "YOU HAVE MY SYMPATHIES"
    ; kind= HurtAsh (3, 3, true)
    ; min_chars= 1 }
  ; { obj_name= "ESCAPE ON THE NARCISSUS"
    ; kind=
        DropItemsAndAssemble
          ( [(CoolantCanister, "DOCKING BAY")]
          , [CatCarrier; Incinerator]
          , "DOCKING BAY" )
    ; min_chars= 1 }
  ; { obj_name= "BLOW IT OUT INTO SPACE"
    ; kind=
        AlienCrewLocationsEncounter
          ("DOCKING BAY", ["AIRLOCK"; "BRIDGE"], encounter_is_alien)
    ; min_chars= 2 }
  ; { obj_name= "WE'RE GOING TO BLOW UP THE SHIP"
    ; kind= SelfDestructAssemble (4, "AIRLOCK", CoolantCanister, 1)
    ; min_chars= 1 }
  ; { obj_name= "CUT OFF EVERY BULKHEAD AND VENT"
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
