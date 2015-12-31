type vector3i = [%json_type "http://ix.io/n2h"]
type char_ptr = [%json_type "http://ix.io/n2i"]
type myrecord = [%json_type "http://ix.io/n2n"]

module Conglomerate = [%json "http://ix.io/n4i"]

let _ =
  let vec : vector3i = (5, ~-1, 3)
  and str : char_ptr = "memes"
  and xyz : myrecord = {x = ~-1567; y = "stuff"}
  and tup : Conglomerate.t = { xs = ["hello"; "world"]; ys = (5, 10) } in
  print_endline @@ Conglomerate.to_string tup;
  let serialized = Conglomerate.to_json tup in
  print_endline @@ Yojson.Safe.pretty_to_string serialized
