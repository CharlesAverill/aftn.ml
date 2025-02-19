(** Random encounters *)

type encounter =
  | Quiet
  | Alien_LostTheSignal
  | Alien_Stalk
  | Alien_Hunt
  | Order937_MeetMeInTheInfirmary
  | Order937_CrewExpendable
  | Order937_CollatingData

let string_of_encounter = function
  | Quiet ->
      "All is quiet"
  | Alien_LostTheSignal ->
      "We've lost the Alien's signal... it must have returned to its nest."
  | Alien_Stalk ->
      "The Alien stalks..."
  | Alien_Hunt ->
      "The Alien hunts!"
  | Order937_MeetMeInTheInfirmary ->
      "All crew, meet in the Infirmary!"
  | Order937_CrewExpendable ->
      "Crew Expendable... the current player loses all scrap."
  | Order937_CollatingData ->
      "Collating Data... the crew has misplaced some scrap."

let encounter_is_alien = function
  | Alien_LostTheSignal | Alien_Stalk | Alien_Hunt ->
      true
  | _ ->
      false

let encounter_is_order937 = function
  | Order937_CollatingData
  | Order937_CrewExpendable
  | Order937_MeetMeInTheInfirmary ->
      true
  | _ ->
      false
