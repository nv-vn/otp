open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Lwt
open Cohttp
open Cohttp_lwt_unix

exception JsonException

type contact =
  { name  : string;
    url   : string;
    email : string }

type license =
  { name : string;
    url  : string } (* Should we make this optional? *)

type info =
  { title   : string;
    descr   : string;
    tos     : string;
    contact : contact;
    license : license;
    version : string }

type record =
  Record of (string * string) list

let otype_of_stype = function
  | ("integer", _) -> "int"
  | ("number", _)  -> "float"
  | ("string", _)  -> "string"
  | ("boolean", _) -> "bool"
  | (s, f)        -> raise JsonException

let get_json url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
    body |> Cohttp_lwt_body.to_string >|= fun body -> body

let yj_to_string = function
  | `Null -> ""
  | any -> Yojson.Basic.Util.to_string any

let record_of_url url = 
  get_json url >>= fun text ->
    let json = Yojson.Basic.from_string text in
    (* Now process the JSON *)
    let open Yojson.Basic.Util in
    let swagger = json |> member "swagger" |> yj_to_string
    and info    = json |> member "info" |> fun info ->
      let title   = info |> member "title" |> yj_to_string
      and descr   = info |> member "description" |> yj_to_string
      and tos     = info |> member "termsOfService" |> yj_to_string
      and contact = info |> member "contact" |> fun contact ->
        let name  = contact |> member "name" |> yj_to_string
        and url   = contact |> member "url" |> yj_to_string
        and email = contact |> member "email" |> yj_to_string
        in {name = name; url = url; email = email}
      and license = info |> member "license" |> fun license ->
        let name = license |> member "name" |> yj_to_string
        and url  = license |> member "url" |> yj_to_string
        in {name = name; url = url}
      and version = info |> member "version" |> yj_to_string
      in {title = title; descr = descr; tos = tos; contact = contact;
          license = license; version = version }
    in return @@ Yojson.Basic.pretty_to_string json

let swagger_mapper argv =
  print_endline "Mapping output";
  print_endline @@ Lwt_unix.run @@ record_of_url "http://petstore.swagger.io/v2/swagger.json";
  print_endline "Finished test request";
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc =
          Pexp_extension ({ txt = "swagger"; loc }, pstr)} ->
        begin match pstr with
        | PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
          Exp.constant ~loc (Const_string (Lwt_unix.run @@ record_of_url sym, None))
        | _ ->
          raise (Location.Error (
                  Location.error ~loc "[%swagger ...] accepts a string, e.g. [%swagger \"http://google.com\"]"))
        end
      | x -> default_mapper.expr mapper x;
  }

let _ = register "swagger" swagger_mapper
