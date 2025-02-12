open Item
open Utils
open Logging
open Map

type objective_kind =
  | BringItemToLocation of (item * string * room option)
      (** Bring [item] to room designated by [string] (loaded into [room option] at runtime) *)
  | CrewAtLocationWithMinimumScrap of (string * room option * int)
      (** Gather all crew at room designated by [string] (loaded into [room option] at runtime) with [int] scrap*)
  | DropCoolant of (int * string * room option)
      (** Drop [int] coolant canisters in the room designated by [string] (loaded into [room option] at runtime)*)

type objective =
  { obj_name: string  (** This objective's name *)
  ; kind: objective_kind  (** What kind of objective *) }

let string_of_objective (o : objective) : string =
  Printf.sprintf "%s - %s" o.obj_name
    ( match o.kind with
    | BringItemToLocation (i, r, _) ->
        Printf.sprintf "Bring one %s to %s" (string_of_item i) r
    | CrewAtLocationWithMinimumScrap (r, _, n) ->
        Printf.sprintf "All Crew members in %s" r
        ^
        if n = 0 then
          ""
        else
          Printf.sprintf
            " with at least %d Scrap in each Crew member's inventory." n
    | DropCoolant (n, s, _) ->
        Printf.sprintf "Drop %d COOLANT CANISTERS in %s" n s )

type final_mission = BlankM

let objectives_stack : objective list =
  [ {obj_name= "PREP SUITS"; kind= DropCoolant (2, "SUIT STORAGE", None)}
  ; { obj_name= "WE'LL TAKE OUR CHANCES IN THE SHUTTLE"
    ; kind= DropCoolant (2, "DOCKING BAY", None) }
  ; { obj_name= "CREW MEETING"
    ; kind= CrewAtLocationWithMinimumScrap ("GALLEY", None, 1) }
  ; { obj_name= "WHAT'S THE DAMAGE?"
    ; kind= CrewAtLocationWithMinimumScrap ("WORKSHOP", None, 0) }
  ; { obj_name= "DRIVE 'EM INTO THE AIRLOCK"
    ; kind= BringItemToLocation (Incinerator, "AIRLOCK", None) }
  ; { obj_name= "WHERE IS IT?"
    ; kind= BringItemToLocation (Flashlight, "MED BAY", None) }
  ; { obj_name= "SHOULDN'T HAVE LANDED ON THIS BALL"
    ; kind= BringItemToLocation (GrappleGun, "GARAGE", None) }
  ; { obj_name= "GIVE IT A LITTLE INCENTIVE"
    ; kind= BringItemToLocation (ElectricProd, "GALLEY", None) }
  ; { obj_name= "ENCOUNTER THE NEST"
    ; kind= BringItemToLocation (Incinerator, "NEST", None) }
  ; { obj_name= "CHECK THE HYPERSLEEP CHAMBER"
    ; kind= BringItemToLocation (MotionTracker, "HYPERSLEEP", None) } ]

let get_objectives (n : int) : objective list =
  if n >= List.length objectives_stack then
    fatal rc_Error "Tried to get more objectives than are available"
  else
    List.take n (shuffle objectives_stack)

let fill_in_obj_room (m : map) (o : objective) : objective =
  match o.kind with
  | DropCoolant (n, s, _) ->
      {o with kind= DropCoolant (n, s, find_room m s)}
  | BringItemToLocation (i, s, _) ->
      {o with kind= BringItemToLocation (i, s, find_room m s)}
  | CrewAtLocationWithMinimumScrap (s, _, i) ->
      {o with kind= CrewAtLocationWithMinimumScrap (s, find_room m s, i)}
