type item =
  | Incinerator
  | Flashlight
  | GrappleGun
  | ElectricProd
  | MotionTracker
  | CoolantCanister
  | CatCarrier

let string_of_item ?(plural : bool = false) (i : item) =
  ( match i with
  | Incinerator ->
      "INCINERATOR"
  | Flashlight ->
      "FLASHLIGHT"
  | GrappleGun ->
      "GRAPPLE GUN"
  | ElectricProd ->
      "ELECTRIC PROD"
  | MotionTracker ->
      "MOTION TRACKER"
  | CoolantCanister ->
      "COOLANT CANISTER"
  | CatCarrier ->
      "CAT CARRIER" )
  ^
  if plural then
    "S"
  else
    ""

(** Which indefinite article to use when referring to the item *)
let article_of_item i =
  if
    List.fold_left
      (fun a prefix ->
        a
        || String.starts_with ~prefix
             (String.lowercase_ascii (string_of_item i)) )
      false ["a"; "e"; "i"; "o"; "u"]
  then
    "an"
  else
    "a"

let cost_of_item = function
  | CatCarrier ->
      1
  | Flashlight ->
      2
  | MotionTracker | GrappleGun | ElectricProd ->
      3
  | Incinerator ->
      4
  | CoolantCanister ->
      -1

let craftable_items =
  [Flashlight; MotionTracker; GrappleGun; ElectricProd; Incinerator]

let item_uses_action = function
  | Flashlight | CoolantCanister ->
      false
  | _ ->
      true
