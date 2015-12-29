open OUnit2

type vector3i = [%json_type "http://ix.io/n2h"]
type char_ptr = [%json_type "http://ix.io/n2i"]

let test_ppx_json_types _ =
  let vec : vector3i = (5, ~-1, 3)
  and str : char_ptr = "memes" in ()

let suite = "Test ppx_json_types" >::: [
    "test_ppx_json_types" >:: test_ppx_json_types
  ]

let _ =
  run_test_tt_main suite
