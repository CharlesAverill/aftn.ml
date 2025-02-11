open Utils
open Logging

type room =
  { (* Name of this room. If None, this room is a corridor *)
    name: string
  ; (* If the room is a corridor *)
    is_corridor: bool
  ; (* How much scrap is in this room *)
    num_scrap: int
  ; (* Whether the room has an event *)
    has_event: bool
  ; (* Items in the room *)
    (* TODO: change to items *)
    items: unit list
  ; (* Connected rooms *)
    connections: room list
  ; (* Room connected by a ladder *)
    ladder_connection: room option }

let blank_room =
  { name= ""
  ; is_corridor= true
  ; num_scrap= 0
  ; has_event= false
  ; items= []
  ; connections= []
  ; ladder_connection= None }

let new_room (name : string) : room = {blank_room with name; is_corridor= false}

type map =
  { (* Name of this map *)
    name: string
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

let map_file_of_map m =
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
    m.name
    (* Room names *)
    (List.fold_left
       (fun s (r : room) ->
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
       "" m.rooms )
    (* Number of corridors *)
    (List.length
       (List.filter
          (fun (r : room) -> String.starts_with ~prefix:"Corridor " r.name)
          m.rooms ) )
    (* Connections *)
    (List.fold_left
       (fun s (r : room) ->
         s ^ r.name ^ ";"
         ^ String.concat ";" (List.map (fun (r : room) -> r.name) r.connections)
         ^ "\n" )
       "" m.rooms )
    (* Ladders *)
    (List.fold_left
       (fun s (r : room) ->
         match r.ladder_connection with
         | None ->
             s
         | Some lc ->
             s ^ r.name ^ ";" ^ lc.name ^ "\n" )
       "" m.rooms )
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

let blank_map =
  { name= ""
  ; rooms= []
  ; player_start_room= blank_room
  ; xeno_start_room= blank_room
  ; ash_start_room= blank_room
  ; scrap_rooms= []
  ; event_rooms= []
  ; coolant_rooms= []
  ; ascii_map= None }

let find_room_by_name (m : map) (name : string) : room =
  let name =
    (* If number, prepend corridor *)
    if
      try
        int_of_string name |> ignore ;
        true
      with Failure _ -> false
    then
      "Corridor " ^ name
    else
      name
  in
  try List.find (fun (r : room) -> r.name = name) m.rooms
  with Not_found -> fatal rc_Error ("Failed to find room \"" ^ name ^ "\"")

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

let parse_map_file (map_fn : string) : map =
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
           print_endline (map_file_of_map m) ;
           (advance_parsing_state state, m)
         ) else
           ( state
           , match state with
             | None ->
                 m
             | Some s -> (
               match s with
               | MapName ->
                   {m with name= line}
               | RoomNames -> (
                   let line' = String.sub line 1 (String.length line - 1) in
                   let room' = new_room line' in
                   match line.[0] with
                   | '*' ->
                       {m with player_start_room= room'; rooms= room' :: m.rooms}
                   | '&' ->
                       {m with xeno_start_room= room'; rooms= room' :: m.rooms}
                   | '$' ->
                       {m with ash_start_room= room'; rooms= room' :: m.rooms}
                   | _ ->
                       {m with rooms= new_room line :: m.rooms} )
               | NumCorridors ->
                   List.fold_left
                     (fun m i ->
                       { m with
                         rooms=
                           {blank_room with name= "Corridor " ^ string_of_int i}
                           :: m.rooms } )
                     m
                     (range 1 (int_of_string line))
               | RoomConnections -> (
                 match String.split_on_char ';' line with
                 | [] ->
                     m
                 | room_name :: connections ->
                     let conns_to_add =
                       List.map
                         (fun c ->
                           (find_room_by_name m room_name, find_room_by_name m c) )
                         connections
                       @ List.map
                           (fun c ->
                             ( find_room_by_name m c
                             , find_room_by_name m room_name ) )
                           connections
                     in
                     List.fold_left
                       (fun m ((r1, r2) : room * room) ->
                         { m with
                           rooms=
                             replace m.rooms
                               (fun r -> r.name = r1.name)
                               {r1 with connections= r2 :: r1.connections} } )
                       m conns_to_add )
               | Ladders -> (
                 match String.split_on_char ';' line with
                 | [first; second] ->
                     let first, second =
                       (find_room_by_name m first, find_room_by_name m second)
                     in
                     { m with
                       rooms=
                         replace
                           (replace m.rooms
                              (fun r -> r.name = first.name)
                              {first with ladder_connection= Some second} )
                           (fun r -> r.name = second.name)
                           {second with ladder_connection= Some first} }
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
                       scrap_rooms= List.map (find_room_by_name m) room_names }
                 )
               | EventRooms -> (
                 match String.split_on_char ';' line with
                 | [] ->
                     m
                 | room_names ->
                     { m with
                       event_rooms= List.map (find_room_by_name m) room_names }
                 )
               | CoolantRooms -> (
                 match String.split_on_char ';' line with
                 | [] ->
                     m
                 | room_names ->
                     { m with
                       coolant_rooms= List.map (find_room_by_name m) room_names
                     } )
               | AsciiMap ->
                   { m with
                     ascii_map=
                       ( match m.ascii_map with
                       | None ->
                           Some line
                       | Some s ->
                           Some (s ^ "\n" ^ line) ) } ) ) )
       (Some MapName, blank_map) (read_file_lines map_fn) )
