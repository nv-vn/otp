open Lwt

module Conglomerate = [%json_extra "http://ix.io/n4i"]

let _ =
  Lwt_unix.run begin
  Conglomerate.from_url "http://ix.io/n5u" >>= fun (`Ok data) ->
    print_endline @@ Conglomerate.to_string data;
    let serialized = Conglomerate.to_json data in
    print_endline @@ Yojson.Safe.pretty_to_string serialized;
    Conglomerate.from_string @@ Yojson.Safe.pretty_to_string serialized;
    return ()
  end
