(** Map parsing and utilities *)

open Utils
open Logging
open Item

type room =
  { name: string  (** Name of this room. If None, this room is a corridor *)
  ; is_corridor: bool  (** If the room is a corridor *)
  ; connections: string list
        (** Connections denoted by room names - required as opposed to [room list] due to functional update of map *)
  ; ladder_connection: string option  (** Room connected by a ladder *) }

(** Get [string] representation of [room] *)
let string_of_room (r : room) =
  Printf.sprintf
    "Name: %s\n\
     Is corridor: %b\n\
     Items: TODO\n\
     Connections: %s\n\
     Ladder connection: %s\n"
    r.name r.is_corridor
    (String.concat "," r.connections)
    (match r.ladder_connection with None -> "None" | Some s -> s)

let blank_room =
  {name= ""; is_corridor= true; connections= []; ladder_connection= None}

(** Create new non-corridor [room] *)
let new_room (name : string) : room = {blank_room with name; is_corridor= false}

type map =
  { (* Name of this map *)
    map_name: string
  ; (* Rooms in this map *)
    rooms: room list
  ; player_start_room: room
  ; xeno_start_room: room
  ; ash_start_room: room
  ; (* Rooms that start with scrap *)
    scrap_rooms: room list
  ; (* Rooms that start with an event *)
    event_rooms: room list
  ; (* Rooms that start with coolant *)
    coolant_rooms: room list
  ; (* ASCII representation of map *)
    ascii_map: string option }

let find_room (m : map) (name : string) : room option =
  List.find_opt (fun r -> r.name = name) m.rooms

(** Get a [string] representation of a [map] that is equivalent to the input map file *)
let map_file_of_map m =
  let find_room x =
    match find_room m x with None -> blank_room | Some r -> r
  in
  Printf.sprintf
    "%s\n\
     ---\n\
     %s\n\
     ---\n\
     %d\n\
     ---\n\
     %s\n\
     ---\n\
     %s\n\
     ---\n\
     %s\n\
     ---\n\
     %s\n\
     ---\n\
     %s\n\
     ---\n\
     %s\n"
    (* Map name *)
    m.map_name
    (* Room names *)
    (String.trim
       (List.fold_left
          (fun s (r : room) ->
            if r.is_corridor then
              s
            else
              s
              ^ ( if r = m.player_start_room then
                    "*"
                  else if r = m.xeno_start_room then
                    "&"
                  else if r = m.ash_start_room then
                    "$"
                  else
                    "" )
              ^ r.name ^ "\n" )
          "" m.rooms ) )
    (* Number of corridors *)
    (List.length (List.filter (fun r -> r.is_corridor) m.rooms))
    (* Connections *)
    (String.trim
       (snd
          (List.fold_left
             (fun (seen, s) (r : room) ->
               ( List.fold_left
                   (fun s (a1, a2) ->
                     fun (x1, x2) ->
                      if (x1.name, x2.name) = (a1.name, a2.name) then
                        true
                      else
                        s (x1, x2) )
                   seen
                   (List.fold_left
                      (fun a x -> (r, x) :: (x, r) :: a)
                      []
                      (List.map find_room r.connections) )
               , let unseen =
                   List.filter
                     (fun x -> not (seen (x, r)))
                     (List.map find_room r.connections)
                 in
                 if unseen = [] then
                   s
                 else
                   s ^ r.name ^ ";"
                   ^ String.concat ";"
                       (List.map (fun (r : room) -> r.name) unseen)
                   ^ "\n" ) )
             ((fun _ -> false), "")
             m.rooms ) ) )
    (* Ladders *)
    (String.trim
       (snd
          (List.fold_left
             (fun (seen, s) r ->
               if seen r then
                 (seen, s)
               else
                 match r.ladder_connection with
                 | None ->
                     (seen, s)
                 | Some lc ->
                     ( (fun x ->
                         if x.name = lc then
                           true
                         else
                           seen x )
                     , s ^ r.name ^ ";" ^ lc ^ "\n" ) )
             ((fun _ -> false), "")
             m.rooms ) ) )
    (* Scrap rooms *)
    (String.concat ";" (List.map (fun (r : room) -> r.name) m.scrap_rooms))
    (* Event rooms *)
    (String.concat ";" (List.map (fun (r : room) -> r.name) m.event_rooms))
    (* Coolant rooms *)
    (String.concat ";" (List.map (fun (r : room) -> r.name) m.coolant_rooms))
    (* ASCII map *)
    ( match m.ascii_map with
    | None ->
        ""
    | Some s ->
        s )

let update_map_room (m : map) (old_room : room) (new_room : room) : map =
  {m with rooms= replace m.rooms (fun r -> r = old_room) new_room}

let blank_map =
  { map_name= ""
  ; rooms= []
  ; player_start_room= blank_room
  ; xeno_start_room= blank_room
  ; ash_start_room= blank_room
  ; scrap_rooms= []
  ; event_rooms= []
  ; coolant_rooms= []
  ; ascii_map= None }

(** Determines which phase of map file parsing is ongoing *)
type map_parsing_state =
  | MapName
  | RoomNames
  | NumCorridors
  | RoomConnections
  | Ladders
  | ScrapRooms
  | EventRooms
  | CoolantRooms
  | AsciiMap

let string_of_parsing_state = function
  | MapName ->
      "Map name"
  | RoomNames ->
      "Room names"
  | NumCorridors ->
      "Number of corridors"
  | RoomConnections ->
      "Room connections"
  | Ladders ->
      "Ladders"
  | ScrapRooms ->
      "Scrap rooms"
  | EventRooms ->
      "Event rooms"
  | CoolantRooms ->
      "Coolant rooms"
  | AsciiMap ->
      "ASCII map"

(** Step parsing state forward *)
let advance_parsing_state = function
  | None ->
      None
  | Some x -> (
    match x with
    | MapName ->
        Some RoomNames
    | RoomNames ->
        Some NumCorridors
    | NumCorridors ->
        Some RoomConnections
    | RoomConnections ->
        Some Ladders
    | Ladders ->
        Some ScrapRooms
    | ScrapRooms ->
        Some EventRooms
    | EventRooms ->
        Some CoolantRooms
    | CoolantRooms ->
        Some AsciiMap
    | AsciiMap ->
        None )

(** Read a map file and parse it into a [map] *)
let parse_map_file (map_fn : string) : map =
  let room_idx, player_start_idx, ash_start_idx, xeno_start_idx =
    (ref 0, ref (-1), ref (-1), ref (-1))
  in
  (* We need to reverse the lists after parsing to maintain lockstep with the input file *)
  let pre_reversed : map =
    snd
      (List.fold_left
         (fun (state, m) line ->
           if line = "---" then (
             _log Log_Debug
               ( "Parsing "
               ^
               match advance_parsing_state state with
               | None ->
                   "nothing"
               | Some state ->
                   string_of_parsing_state state ) ;
             (advance_parsing_state state, m)
           ) else
             ( state
             , match state with
               | None ->
                   m
               | Some s -> (
                 match s with
                 | MapName ->
                     {m with map_name= line}
                 | RoomNames ->
                     let line' = String.sub line 1 (String.length line - 1) in
                     let room =
                       match line.[0] with
                       | '*' ->
                           player_start_idx := !room_idx ;
                           new_room line'
                       | '&' ->
                           xeno_start_idx := !room_idx ;
                           new_room line'
                       | '$' ->
                           ash_start_idx := !room_idx ;
                           new_room line'
                       | _ ->
                           new_room line
                     in
                     room_idx := !room_idx + 1 ;
                     {m with rooms= room :: m.rooms}
                 | NumCorridors ->
                     List.fold_left
                       (fun m i ->
                         { m with
                           rooms=
                             { blank_room with
                               name= string_of_int i
                             ; is_corridor= true }
                             :: m.rooms } )
                       m
                       (range 1 (int_of_string line))
                 | RoomConnections -> (
                   match String.split_on_char ';' line with
                   | [] ->
                       m
                   | room_name :: connections ->
                       List.fold_left
                         (fun m r2_name ->
                           match
                             (find_room m room_name, find_room m r2_name)
                           with
                           | Some r1, Some r2 ->
                               update_map_room
                                 (update_map_room m r1
                                    { r1 with
                                      connections= r1.connections @ [r2.name] } )
                                 r2
                                 { r2 with
                                   connections= r2.connections @ [r1.name] }
                           | _, _ ->
                               fatal rc_Error
                                 ( "Tried adding connection to non-declared \
                                    room " ^ r2_name ) )
                         m connections )
                 | Ladders -> (
                   match String.split_on_char ';' line with
                   | [first; second] -> (
                     match (find_room m first, find_room m second) with
                     | Some first, Some second ->
                         { m with
                           rooms=
                             replace
                               (replace m.rooms
                                  (fun r -> r.name = first.name)
                                  { first with
                                    ladder_connection= Some second.name } )
                               (fun r -> r.name = second.name)
                               {second with ladder_connection= Some first.name}
                         }
                     | _ ->
                         fatal rc_Error
                           ( "Tried to add invalid ladder connection: " ^ first
                           ^ " - " ^ second ) )
                   | _ ->
                       _log Log_Warning
                         ("Failed to parse ladders line: \"" ^ line ^ "\"") ;
                       m )
                 | ScrapRooms -> (
                   match String.split_on_char ';' line with
                   | [] ->
                       m
                   | room_names ->
                       { m with
                         scrap_rooms=
                           List.map
                             (fun x ->
                               match find_room m x with
                               | None ->
                                   _log Log_Warning
                                     ( "Failed to find room " ^ x
                                     ^ " in map while determining scrap rooms"
                                     ) ;
                                   blank_room
                               | Some x ->
                                   x )
                             room_names } )
                 | EventRooms -> (
                   match String.split_on_char ';' line with
                   | [] ->
                       m
                   | room_names ->
                       { m with
                         event_rooms=
                           List.map
                             (fun x ->
                               match find_room m x with
                               | None ->
                                   _log Log_Warning
                                     ( "Failed to find room " ^ x
                                     ^ " in map while determining event rooms"
                                     ) ;
                                   blank_room
                               | Some x ->
                                   x )
                             room_names } )
                 | CoolantRooms -> (
                   match String.split_on_char ';' line with
                   | [] ->
                       m
                   | room_names ->
                       { m with
                         coolant_rooms=
                           List.map
                             (fun x ->
                               match find_room m x with
                               | None ->
                                   _log Log_Warning
                                     ( "Failed to find room " ^ x
                                     ^ " in map while determining coolant rooms"
                                     ) ;
                                   blank_room
                               | Some x ->
                                   x )
                             room_names } )
                 | AsciiMap ->
                     { m with
                       ascii_map=
                         ( match m.ascii_map with
                         | None ->
                             Some line
                         | Some s ->
                             Some (s ^ "\n" ^ line) ) } ) ) )
         (Some MapName, blank_map) (read_file_lines map_fn) )
  in
  (* (
    (fun m -> (fun r -> List.map (fun c -> List.find_index (fun x -> x.name = ) m.rooms ) r.connections))
  ); *)
  { pre_reversed with
    rooms= List.rev pre_reversed.rooms
  ; player_start_room=
      List.nth pre_reversed.rooms
        (List.length pre_reversed.rooms - !player_start_idx - 1)
  ; ash_start_room=
      List.nth pre_reversed.rooms
        (List.length pre_reversed.rooms - !ash_start_idx - 1)
  ; xeno_start_room=
      List.nth pre_reversed.rooms
        (List.length pre_reversed.rooms - !xeno_start_idx - 1) }
