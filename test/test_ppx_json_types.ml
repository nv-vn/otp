module Conglomerate = [%json "http://ix.io/n4i"]

let _ =
  let tup : Conglomerate.t = { xs = ["hello"; "world"]; ys = (5, 10) } in
  print_endline @@ Conglomerate.to_string tup;
  let serialized = Conglomerate.to_json tup in
  print_endline @@ Yojson.Safe.pretty_to_string serialized
