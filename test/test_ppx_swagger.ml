open OUnit2

let test_ppx_swagger _ =
  print_endline [%swagger "http://petstore.swagger.io/v2/swagger.json"]

let suite = "Test ppx_swagger" >::: [
    "test_ppx_swagger" >:: test_ppx_swagger
  ]

let _ =
  run_test_tt_main suite
