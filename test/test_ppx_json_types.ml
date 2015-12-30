open OUnit2

type vector3i = [%json_type "http://ix.io/n2h"]
type char_ptr = [%json_type "http://ix.io/n2i"]
type myrecord = [%json_type "http://ix.io/n2n"]

module Conglomerate = [%json "http://ix.io/n3w"]

let test_ppx_json_types _ =
  let vec : vector3i = (5, ~-1, 3)
  and str : char_ptr = "memes"
  and xyz : myrecord = {x = ~-1567; y = "stuff"}
  and tup : Conglomerate.Hide.t = ([1; 2; 3], 4.0, {str = "five"}) in ()

let suite = "Test ppx_json_types" >::: [
    "test_ppx_json_types" >:: test_ppx_json_types
  ]

let _ =
  run_test_tt_main suite
