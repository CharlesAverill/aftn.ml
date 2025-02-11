open AFTN.Map

let () =
  print_endline
    (map_file_of_map
       (parse_map_file "/home/charles/Desktop/aftn.ml/game_data/maps/default") )
