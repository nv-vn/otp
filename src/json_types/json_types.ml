open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Lwt
open Cohttp
open Cohttp_lwt_unix

exception JsonException

type json_type =
  | Record of (string * string) list
  | Union of (string * json_type) list
  | Product of string list
  | Alias of string

let get_json url =
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
    body |> Cohttp_lwt_body.to_string >|= fun body -> body

let yj_to_string = function
  | `Null -> ""
  | any -> Yojson.Basic.Util.to_string any

let rec make_record json loc =
  let open Yojson.Basic.Util in
  let items = to_list json in
  make_alias json loc

and make_union json loc =
  make_product json loc

and make_product json loc =
  let open Yojson.Basic.Util in
  let items = to_list json |> filter_string in
  Typ.tuple ~loc (List.map (fun item -> Typ.constr ~loc {txt = Lident item; loc = loc} []) items)

and make_alias json loc =
  let open Yojson.Basic.Util in
  let name = to_string json in
  Typ.constr ~loc {txt = Lident name; loc = loc} []

let type_of_url url loc =
  get_json url >>= fun text ->
    let json = Yojson.Basic.from_string text in
    (* Now process the JSON *)
    let open Yojson.Basic.Util in
    let record  = json |> member "record"
    and union   = json |> member "union"
    and product = json |> member "product"
    and alias   = json |> member "alias" in
    return @@ match (record, union, product, alias) with
    | (record, `Null, `Null, `Null)  -> make_record  record  loc
    | (`Null, union, `Null, `Null)   -> make_union   union   loc
    | (`Null, `Null, product, `Null) -> make_product product loc
    | (`Null, `Null, `Null, alias)   -> make_alias   alias   loc
    | (_, _, _, _) -> make_alias (`String "int") loc

let json_type_mapper argv =
  { default_mapper with
    typ = fun mapper typ ->
      match typ with
      | { ptyp_desc =
          Ptyp_extension ({ txt = "json_type"; loc }, ptyp); _} ->
        begin match ptyp with
        | PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_constant (Const_string (sym, None))}, _)}] ->
          Lwt_unix.run @@ type_of_url sym loc
        | _ ->
          raise (Location.Error (
                  Location.error ~loc "[%json_type ...] accepts a string, e.g. [%json_type \"http://google.com\"]"))
        end
      | x -> default_mapper.typ mapper x;
  }

let _ = register "json_type" json_type_mapper
