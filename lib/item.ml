type item =
  | Incinerator
  | Flashlight
  | GrappleGun
  | ElectricProd
  | MotionTracker

let string_of_item = function
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
